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

  private dock!: THREE.Mesh;
  private water!: THREE.Mesh;
  private waterMaterial!: THREE.MeshStandardMaterial;
  private resizeObserver!: ResizeObserver;
  private textureLoader: THREE.TextureLoader = new THREE.TextureLoader();

  private getAspectRatio(): number {
    return this.canvas.clientWidth / this.canvas.clientHeight;
  }

  private createPortStructure(): void {
    const dockMaterial = new THREE.MeshStandardMaterial({ color: 0x8B7355 });
    const storageMaterial = new THREE.MeshStandardMaterial({ color: 0xFFD700 }); // Yellow storage areas
    const greenMaterial = new THREE.MeshStandardMaterial({ color: 0x90EE90 }); // Green areas
    
    // Main central dock area
    const mainDock = new THREE.BoxGeometry(50, 0.5, 30);
    const mainDockMesh = new THREE.Mesh(mainDock, dockMaterial);
    mainDockMesh.position.set(0, 0, 0);
    this.scene.add(mainDockMesh);

    // Upper left peninsula
    const upperLeftPeninsula = new THREE.BoxGeometry(25, 0.5, 20);
    const upperLeftMesh = new THREE.Mesh(upperLeftPeninsula, dockMaterial);
    upperLeftMesh.position.set(-30, 0, 20);
    this.scene.add(upperLeftMesh);

    // Upper right peninsula
    const upperRightPeninsula = new THREE.BoxGeometry(20, 0.5, 15);
    const upperRightMesh = new THREE.Mesh(upperRightPeninsula, dockMaterial);
    upperRightMesh.position.set(30, 0, 22);
    this.scene.add(upperRightMesh);

    // Lower left area
    const lowerLeftArea = new THREE.BoxGeometry(30, 0.5, 25);
    const lowerLeftMesh = new THREE.Mesh(lowerLeftArea, dockMaterial);
    lowerLeftMesh.position.set(-35, 0, -20);
    this.scene.add(lowerLeftMesh);

    // Lower right area
    const lowerRightArea = new THREE.BoxGeometry(25, 0.5, 20);
    const lowerRightMesh = new THREE.Mesh(lowerRightArea, dockMaterial);
    lowerRightMesh.position.set(32, 0, -18);
    this.scene.add(lowerRightMesh);

    // Storage areas (yellow)
    this.createStorageArea(storageMaterial, -10, 0, 5, 8, 0.6, 6);
    this.createStorageArea(storageMaterial, 10, 0, -5, 10, 0.6, 8);
    this.createStorageArea(storageMaterial, -30, 0, 18, 12, 0.6, 10);
    this.createStorageArea(storageMaterial, 28, 0, 20, 8, 0.6, 7);

    // Green perimeter areas
    this.createStorageArea(greenMaterial, -45, 0, 20, 10, 0.4, 15);
    this.createStorageArea(greenMaterial, 40, 0, 22, 8, 0.4, 12);
    this.createStorageArea(greenMaterial, -50, 0, -20, 12, 0.4, 20);
    this.createStorageArea(greenMaterial, 42, 0, -18, 10, 0.4, 15);

    // Connecting piers
    this.createPier(dockMaterial, -15, 0, 10, 3, 0.3, 8);
    this.createPier(dockMaterial, 15, 0, 12, 3, 0.3, 10);
    this.createPier(dockMaterial, -20, 0, -10, 3, 0.3, 6);
    this.createPier(dockMaterial, 18, 0, -8, 3, 0.3, 7);
  }

  private createStorageArea(material: THREE.Material, x: number, y: number, z: number, 
                           width: number, height: number, depth: number): void {
    const geometry = new THREE.BoxGeometry(width, height, depth);
    const mesh = new THREE.Mesh(geometry, material);
    mesh.position.set(x, y, z);
    this.scene.add(mesh);
  }

  private createPier(material: THREE.Material, x: number, y: number, z: number, 
                    width: number, height: number, depth: number): void {
    const geometry = new THREE.BoxGeometry(width, height, depth);
    const mesh = new THREE.Mesh(geometry, material);
    mesh.position.set(x, y, z);
    this.scene.add(mesh);
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
    
    // Animate water normal map
    if (this.waterMaterial.normalMap) {
      this.waterMaterial.normalMap.offset.x += 0.0009;
      this.waterMaterial.normalMap.offset.y += 0.0006;
    }
    
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

    // Setup ResizeObserver to handle zoom and window resize
    this.resizeObserver = new ResizeObserver(() => {
      this.onWindowResize();
    });
    this.resizeObserver.observe(this.canvas);

    this.animateScene();
  }

  ngOnDestroy(): void {
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