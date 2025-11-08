import { Component, AfterViewInit, ElementRef, ViewChild, Input } from '@angular/core';
import * as THREE from 'three';

@Component({
  selector: 'app-dock-view',
  standalone: true,
  imports: [],
  templateUrl: './dock-view.html',
  styleUrl: './dock-view.scss',
})
export class DockView implements AfterViewInit {
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

  private dock!: THREE.Mesh;
  private water!: THREE.Mesh;

  private getAspectRatio(): number {
    return this.canvas.clientWidth / this.canvas.clientHeight;
  }

  private createScene(): void {
    // Scene setup
    this.scene.background = new THREE.Color(0x87CEEB); // Sky blue

    // Dock platform
    const dockGeometry = new THREE.BoxGeometry(10, 0.5, 5);
    const dockMaterial = new THREE.MeshBasicMaterial({ color: 0x8B4513 });
    this.dock = new THREE.Mesh(dockGeometry, dockMaterial);
    this.dock.position.y = 0;
    this.scene.add(this.dock);

    // Water plane
    const waterGeometry = new THREE.PlaneGeometry(50, 50);
    const waterMaterial = new THREE.MeshBasicMaterial({ 
      color: 0x1E90FF,
      side: THREE.DoubleSide 
    });
    this.water = new THREE.Mesh(waterGeometry, waterMaterial);
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
    this.renderer.render(this.scene, this.camera);
  }

  ngAfterViewInit(): void {
    this.createScene();
    this.renderer = new THREE.WebGLRenderer({ canvas: this.canvas });
    this.renderer.setPixelRatio(devicePixelRatio);
    this.renderer.setSize(this.canvas.clientWidth, this.canvas.clientHeight);
    this.animateScene();
  }
}