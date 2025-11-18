import { Component, AfterViewInit, ElementRef, ViewChild, Input, OnDestroy } from '@angular/core';
import * as THREE from 'three';
import { OrbitControls } from 'three/examples/jsm/controls/OrbitControls.js';

import { DockBuilder } from '../scene/DockBuilder';
import { PortBuilder } from '../scene/PortBuilder';

// NOVOS IMPORTS
import { WarehouseBuilder } from '../scene/WarehouseBuilder';
import { YardBuilder } from '../scene/YardBuilder';
import { ContainerBuilder } from '../scene/ContainerBuilder';

import { StsCraneBuilder } from '../scene/StsCraneBuilder';
import { StorageAreaBuilder, StorageAreaType } from '../scene/StorageAreaBuilder';

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

  private get canvas(): HTMLCanvasElement {
    return this.canvasRef.nativeElement;
  }

  private renderer!: THREE.WebGLRenderer;
  private scene: THREE.Scene = new THREE.Scene();
  private camera!: THREE.PerspectiveCamera;
  private controls!: OrbitControls;

  // === FIX IMPORTANTE ===
  private docks: THREE.Group[] = [];

  private water!: THREE.Mesh;
  private waterMaterial!: THREE.MeshStandardMaterial;

  private resizeObserver!: ResizeObserver;
  private textureLoader: THREE.TextureLoader = new THREE.TextureLoader();
  private raycaster: THREE.Raycaster = new THREE.Raycaster();
  private mouse: THREE.Vector2 = new THREE.Vector2();

  private targetPosition: THREE.Vector3 = new THREE.Vector3();
  private currentTarget: THREE.Vector3 = new THREE.Vector3();
  private isAnimating = false;
  private animationProgress = 0;
  private animationDuration = 1.5;
  private clock: THREE.Clock = new THREE.Clock();


  private createPortStructure(): void {

    const port = PortBuilder.createPort(200, 150);
    port.position.set(0, 0, 0);
    this.scene.add(port);

    // === Docks ===
    const dockOffsetZ = -75;
    const dockY = 0.5;

    const dock1 = DockBuilder.createDock(30, 10, new THREE.Vector3(-60, dockY, dockOffsetZ), 1);
    const dock2 = DockBuilder.createDock(30, 10, new THREE.Vector3(0, dockY, dockOffsetZ), 2);
    const dock3 = DockBuilder.createDock(30, 10, new THREE.Vector3(60, dockY, dockOffsetZ), 3);

    this.scene.add(dock1, dock2, dock3);
    this.docks.push(dock1, dock2, dock3);

    // === Warehouse ===
    const warehouse1 = WarehouseBuilder.createWarehouse(
      40, 25,
      new THREE.Vector3(40, 0, 25),
      1
    );
    this.scene.add(warehouse1);

    // === Yard ===
    const yard1 = YardBuilder.createYard(
      60, 40,
      new THREE.Vector3(-40, 0, 30),
      1
    );

    this.scene.add(yard1);

    // === STS Cranes ===
    const crane1 = StsCraneBuilder.createCrane(
      6, 25, 20,
      new THREE.Vector3(-60, 0, dockOffsetZ + 10),
      1
    );
    crane1.rotation.y = Math.PI / 2;

    const crane2 = StsCraneBuilder.createCrane(
      6, 25, 20,
      new THREE.Vector3(0, 0, dockOffsetZ + 10),
      2
    );
    crane2.rotation.y = Math.PI / 2;

    const crane3 = StsCraneBuilder.createCrane(
      6, 25, 20,
      new THREE.Vector3(60, 0, dockOffsetZ + 10),
      3
    );
    crane3.rotation.y = Math.PI / 2;

    this.scene.add(crane1, crane2, crane3);
  }


  private onCanvasClick(event: MouseEvent): void {

    const rect = this.canvas.getBoundingClientRect();
    this.mouse.x = ((event.clientX - rect.left) / rect.width) * 2 - 1;
    this.mouse.y = -((event.clientY - rect.top) / rect.height) * 2 + 1;

    this.raycaster.setFromCamera(this.mouse, this.camera);

    // IMPORTANTE: detectar sub-mesh dentro do Group
    const intersects = this.raycaster.intersectObjects(this.docks, true);

    if (intersects.length === 0) return;

    const clickedMesh = intersects[0].object;
    const clickedDock = clickedMesh.parent as THREE.Group;

    const dockPos = clickedDock.position;

    this.currentTarget.copy(this.controls.target);
    this.targetPosition.copy(dockPos);
    this.isAnimating = true;
    this.animationProgress = 0;

    this.highlightDock(clickedDock);
  }


  private highlightDock(dock: THREE.Group): void {
    this.docks.forEach(d => {
      d.traverse(obj => {
        if (obj instanceof THREE.Mesh) {
          (obj.material as THREE.MeshStandardMaterial).color.setHex(0x777777);
        }
      });
    });

    dock.traverse(obj => {
      if (obj instanceof THREE.Mesh) {
        (obj.material as THREE.MeshStandardMaterial).color.setHex(0xffd700);
      }
    });
  }



  private easeInOutCubic(t: number): number {
    return t < 0.5 ? 4 * t * t * t : 1 - Math.pow(-2 * t + 2, 3) / 2;
  }


  private updateCameraAnimation(dt: number): void {
    if (!this.isAnimating) return;

    this.animationProgress += dt / this.animationDuration;

    const t = Math.min(this.animationProgress, 1);
    const eased = this.easeInOutCubic(t);

    this.controls.target.lerpVectors(this.currentTarget, this.targetPosition, eased);
    this.controls.update();

    if (t >= 1) this.isAnimating = false;
  }


  private createScene(): void {

    this.scene.background = new THREE.Color(0xffffff);

    const ambient = new THREE.AmbientLight(0xffffff, 0.8);
    this.scene.add(ambient);

    const sun = new THREE.DirectionalLight(0xffffff, 0.6);
    sun.position.set(10, 10, 5);
    this.scene.add(sun);

    this.createPortStructure();

    const waterGeo = new THREE.PlaneGeometry(500, 500);
    const waterNormals = this.textureLoader.load('assets/textures/water/waternormals.jpg');
    waterNormals.wrapS = waterNormals.wrapT = THREE.RepeatWrapping;
    waterNormals.repeat.set(50, 50);

    this.waterMaterial = new THREE.MeshStandardMaterial({
      color: 0x1e90ff,
      normalMap: waterNormals,
      normalScale: new THREE.Vector2(0.5, 0.5),
      roughness: 0.4,
      metalness: 0.1,
      transparent: true,
      opacity: 0.7,
      side: THREE.DoubleSide
    });

    this.water = new THREE.Mesh(waterGeo, this.waterMaterial);
    this.water.rotation.x = -Math.PI / 2;
    this.water.position.y = -0.5;
    this.scene.add(this.water);

    const aspect = this.canvas.clientWidth / this.canvas.clientHeight;
    this.camera = new THREE.PerspectiveCamera(
      this.fieldOfView,
      aspect,
      this.nearClippingPane,
      this.farClippingPane
    );
    this.camera.position.set(0, 8, this.cameraZ);
    this.camera.lookAt(0, 0, 0);
  }


  private animateScene(): void {
    requestAnimationFrame(() => this.animateScene());
    const dt = this.clock.getDelta();

    if (this.waterMaterial.normalMap) {
      this.waterMaterial.normalMap.offset.x += 0.0009;
      this.waterMaterial.normalMap.offset.y += 0.0006;
    }

    this.updateCameraAnimation(dt);
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

    this.controls = new OrbitControls(this.camera, this.canvas);
    this.controls.enableDamping = true;
    this.controls.dampingFactor = 0.05;
    this.controls.screenSpacePanning = false;
    this.controls.minDistance = 5;
    this.controls.maxDistance = 50;
    this.controls.maxPolarAngle = Math.PI / 2;

    this.canvas.addEventListener('click', (event) => this.onCanvasClick(event));

    this.resizeObserver = new ResizeObserver(() => this.onWindowResize());
    this.resizeObserver.observe(this.canvas);

    this.animateScene();
  }


  ngOnDestroy(): void {
    if (this.canvas) {
      this.canvas.removeEventListener('click', (e) => this.onCanvasClick(e));
    }
    if (this.resizeObserver) this.resizeObserver.disconnect();
    if (this.controls) this.controls.dispose();
    if (this.renderer) this.renderer.dispose();
  }
}
