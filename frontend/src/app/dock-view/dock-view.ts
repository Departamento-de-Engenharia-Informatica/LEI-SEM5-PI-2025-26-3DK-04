import { Component, AfterViewInit, ElementRef, ViewChild, Input, OnDestroy } from '@angular/core';
import * as THREE from 'three';
import { OrbitControls } from 'three/examples/jsm/controls/OrbitControls.js';

@Component({
  selector: 'app-dock-view',
  standalone: true,
  imports: [],
  templateUrl: './dock-view.html',
  styleUrl: './dock-view.scss',
})
export class DockView implements AfterViewInit, OnDestroy {
  @ViewChild('myCanvas') private canvasRef!: ElementRef;

  //* Stage Properties
  @Input() public cameraZ: number = 20;
  @Input() public fieldOfView: number = 60;
  @Input('nearClipping') public nearClippingPane: number = 1;
  @Input('farClipping') public farClippingPane: number = 1000;

  //? Helper Properties
  private get canvas(): HTMLCanvasElement {
    return this.canvasRef.nativeElement;
  }

  private renderer!: THREE.WebGLRenderer;
  private scene: THREE.Scene = new THREE.Scene();
  private camera!: THREE.PerspectiveCamera;
  private controls!: OrbitControls;

  private docks: THREE.Mesh[] = [];
  private water!: THREE.Mesh;
  private waterMaterial!: THREE.MeshStandardMaterial;
  private resizeObserver!: ResizeObserver;
  private textureLoader: THREE.TextureLoader = new THREE.TextureLoader();
  private raycaster: THREE.Raycaster = new THREE.Raycaster();
  private mouse: THREE.Vector2 = new THREE.Vector2();
  
  // Animation properties
  private targetPosition: THREE.Vector3 = new THREE.Vector3();
  private currentTarget: THREE.Vector3 = new THREE.Vector3();
  private isAnimating: boolean = false;
  private animationProgress: number = 0;
  private animationDuration: number = 1.5; // seconds
  private clock: THREE.Clock = new THREE.Clock();

  private getAspectRatio(): number {
    return this.canvas.clientWidth / this.canvas.clientHeight;
  }

  private createPortStructure(): void {
    // Create 3 distinct docks
    const dockMaterial1 = new THREE.MeshStandardMaterial({ color: 0x8B7355 });
    const dockMaterial2 = new THREE.MeshStandardMaterial({ color: 0xA0826D });
    const dockMaterial3 = new THREE.MeshStandardMaterial({ color: 0x9B7653 });
    
    // Dock 1 - Left
    const dock1Geometry = new THREE.BoxGeometry(30, 1, 20);
    const dock1 = new THREE.Mesh(dock1Geometry, dockMaterial1);
    dock1.position.set(-40, 0, 0);
    dock1.name = 'Dock 1';
    dock1.userData = { dockId: 1 };
    this.scene.add(dock1);
    this.docks.push(dock1);

    // Dock 2 - Center
    const dock2Geometry = new THREE.BoxGeometry(30, 1, 20);
    const dock2 = new THREE.Mesh(dock2Geometry, dockMaterial2);
    dock2.position.set(0, 0, 0);
    dock2.name = 'Dock 2';
    dock2.userData = { dockId: 2 };
    this.scene.add(dock2);
    this.docks.push(dock2);

    // Dock 3 - Right
    const dock3Geometry = new THREE.BoxGeometry(30, 1, 20);
    const dock3 = new THREE.Mesh(dock3Geometry, dockMaterial3);
    dock3.position.set(40, 0, 0);
    dock3.name = 'Dock 3';
    dock3.userData = { dockId: 3 };
    this.scene.add(dock3);
    this.docks.push(dock3);
  }

  private onCanvasClick(event: MouseEvent): void {
    // Calculate mouse position in normalized device coordinates (-1 to +1)
    const rect = this.canvas.getBoundingClientRect();
    this.mouse.x = ((event.clientX - rect.left) / rect.width) * 2 - 1;
    this.mouse.y = -((event.clientY - rect.top) / rect.height) * 2 + 1;

    // Update the raycaster with the camera and mouse position
    this.raycaster.setFromCamera(this.mouse, this.camera);

    // Check for intersections with the docks
    const intersects = this.raycaster.intersectObjects(this.docks);

    if (intersects.length > 0) {
      const clickedDock = intersects[0].object as THREE.Mesh;
      const dockPosition = clickedDock.position;
      
      // Start smooth animation to new target
      this.currentTarget.copy(this.controls.target);
      this.targetPosition.copy(dockPosition);
      this.isAnimating = true;
      this.animationProgress = 0;

      console.log(`Clicked on ${clickedDock.name} at position:`, dockPosition);
      
      // Add visual feedback
      this.highlightDock(clickedDock);
    }
  }

  private highlightDock(dock: THREE.Mesh): void {
    // Reset all docks to normal
    this.docks.forEach((d, index) => {
      const material = d.material as THREE.MeshStandardMaterial;
      if (index === 0) material.color.setHex(0x8B7355);
      else if (index === 1) material.color.setHex(0xA0826D);
      else material.color.setHex(0x9B7653);
    });

    // Highlight the selected dock
    const material = dock.material as THREE.MeshStandardMaterial;
    material.color.setHex(0xFFD700); // Gold color for selected dock
  }

  // Easing function for smooth animation (ease-in-out)
  private easeInOutCubic(t: number): number {
    return t < 0.5 ? 4 * t * t * t : 1 - Math.pow(-2 * t + 2, 3) / 2;
  }

  private updateCameraAnimation(deltaTime: number): void {
    if (!this.isAnimating) return;

    this.animationProgress += deltaTime / this.animationDuration;

    if (this.animationProgress >= 1) {
      this.animationProgress = 1;
      this.isAnimating = false;
    }

    const easedProgress = this.easeInOutCubic(this.animationProgress);
    
    // Interpolate between current and target position
    this.controls.target.lerpVectors(
      this.currentTarget,
      this.targetPosition,
      easedProgress
    );
    
    this.controls.update();
  }

  private createScene(): void {
    // Scene setup
    this.scene.background = new THREE.Color(0xFFFFFF);

    // Add lights for the water material
    const ambientLight = new THREE.AmbientLight(0xffffff, 0.8);
    this.scene.add(ambientLight);

    const directionalLight = new THREE.DirectionalLight(0xffffff, 0.6);
    directionalLight.position.set(10, 10, 5);
    this.scene.add(directionalLight);

    // Create Amsterdam Port layout
    this.createPortStructure();

    // Water plane with normal map
    const waterGeometry = new THREE.PlaneGeometry(500, 500);
    
    // Load water normal texture
    const waterNormals = this.textureLoader.load('assets/textures/water/waternormals.jpg');
    waterNormals.wrapS = waterNormals.wrapT = THREE.RepeatWrapping;
    waterNormals.repeat.set(50, 50);

    this.waterMaterial = new THREE.MeshStandardMaterial({ 
      color: 0x1E90FF,
      normalMap: waterNormals,
      normalScale: new THREE.Vector2(0.5, 0.5),
      roughness: 0.4,
      metalness: 0.1,
      transparent: true,
      opacity: 0.7,
      side: THREE.DoubleSide 
    });
    
    this.water = new THREE.Mesh(waterGeometry, this.waterMaterial);
    this.water.rotation.x = -Math.PI / 2;
    this.water.position.y = -0.5;
    this.scene.add(this.water);

    // Camera
    const aspectRatio = this.getAspectRatio();
    this.camera = new THREE.PerspectiveCamera(
      this.fieldOfView,
      aspectRatio,
      this.nearClippingPane,
      this.farClippingPane
    );
    this.camera.position.set(0, 8, this.cameraZ);
    this.camera.lookAt(0, 0, 0);
  }

  private animateScene(): void {
    requestAnimationFrame(() => this.animateScene());
    
    const deltaTime = this.clock.getDelta();
    
    // Animate water normal map
    if (this.waterMaterial.normalMap) {
      this.waterMaterial.normalMap.offset.x += 0.0009;
      this.waterMaterial.normalMap.offset.y += 0.0006;
    }
    
    // Update camera animation
    this.updateCameraAnimation(deltaTime);
    
    this.controls.update();
    this.renderer.render(this.scene, this.camera);
  }

  private onWindowResize(): void {
    const width = this.canvas.clientWidth;
    const height = this.canvas.clientHeight;
    
    this.camera.aspect = width / height;
    this.camera.updateProjectionMatrix();
    this.renderer.setSize(width, height, false);
  }

  ngAfterViewInit(): void {
    this.createScene();
    this.renderer = new THREE.WebGLRenderer({ 
      canvas: this.canvas,
      antialias: true,
      alpha: true
    });
    this.renderer.setPixelRatio(Math.min(window.devicePixelRatio, 2));
    this.renderer.setSize(this.canvas.clientWidth, this.canvas.clientHeight, false);

    // Setup OrbitControls
    this.controls = new OrbitControls(this.camera, this.canvas);
    this.controls.enableDamping = true;
    this.controls.dampingFactor = 0.05;
    this.controls.screenSpacePanning = false;
    this.controls.minDistance = 5;
    this.controls.maxDistance = 50;
    this.controls.maxPolarAngle = Math.PI / 2;

    // Add click event listener
    this.canvas.addEventListener('click', (event) => this.onCanvasClick(event));

    // Setup ResizeObserver to handle zoom and window resize
    this.resizeObserver = new ResizeObserver(() => {
      this.onWindowResize();
    });
    this.resizeObserver.observe(this.canvas);

    this.animateScene();
  }

  ngOnDestroy(): void {
    if (this.canvas) {
      this.canvas.removeEventListener('click', (event) => this.onCanvasClick(event));
    }
    if (this.resizeObserver) {
      this.resizeObserver.disconnect();
    }
    if (this.controls) {
      this.controls.dispose();
    }
    if (this.renderer) {
      this.renderer.dispose();
    }
  }
}