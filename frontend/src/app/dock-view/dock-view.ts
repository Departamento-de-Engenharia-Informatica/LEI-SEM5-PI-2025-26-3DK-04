import { Component, AfterViewInit, ElementRef, ViewChild, Input, OnDestroy } from '@angular/core';
import * as THREE from 'three';
import { OrbitControls } from 'three/examples/jsm/controls/OrbitControls.js';
import { firstValueFrom } from 'rxjs';
import { DockBuilder } from '../scene/DockBuilder';
import { PortBuilder } from '../scene/PortBuilder';
import { AdminService } from '../admin/admin.service'; // JÁ IMPORTADO
// NOVOS IMPORTS
import { WarehouseBuilder } from '../scene/WarehouseBuilder';
import { YardBuilder } from '../scene/YardBuilder';
import { YardGantryCraneBuilder, YardCraneType } from '../scene/YardGantryCraneBuilder';
// import { ContainerBuilder } from '../scene/ContainerBuilder'; // Não é usado diretamente aqui
import { StsCraneBuilder } from '../scene/StsCraneBuilder'; // Não é usado diretamente aqui
import { StorageAreaBuilder, StorageAreaType } from '../scene/StorageAreaBuilder'; // Importado mas não usado diretamente, mantido por segurança
import { VesselBuilder } from '../scene/VesselBuilder';

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

  private movementSpeed = 1.0;
  private movement = {
    forward: false,
    backward: false,
    left: false,
    right: false,
    up: false,
    down: false
  };

  // 1. INJEÇÃO DO SERVIÇO
  constructor(private adminService: AdminService) {}


  // 2. NOVO MÉTODO PARA CARREGAR DADOS ASINCRONAMENTE
  private async loadPortStructure(): Promise<void> {
    try {
      const docks = await firstValueFrom(this.adminService.getAllDocks());
      const storageAreas = await firstValueFrom(this.adminService.getAllStorageAreas());
      const resources = await firstValueFrom(this.adminService.getAllPhysicalResources());

      const yards = storageAreas.filter(a => a.storageAreaType === "Yard");
      const warehouses = storageAreas.filter(a => a.storageAreaType === "Warehouse");

      const stsCranes = resources.filter(r => r.type === "STS_CRANE");
      console.log(stsCranes);
      const yardCranes = resources.filter(r => r.type === "YARD_CRANE");
      console.log(yardCranes);

      const approvedVesselvisits = await firstValueFrom(this.adminService.getApprovedVesselVisitNotifications());
      console.log(approvedVesselvisits);

      const vessels = await firstValueFrom(this.adminService.getVessels());
      console.log(vessels);

      const vesselTypes = await firstValueFrom(this.adminService.getVesselTypes());
      console.log(vesselTypes);

      await this.createPortStructure(
        docks,
        yards,
        warehouses,
        stsCranes,
        yardCranes,
        approvedVesselvisits,
        vessels,
        vesselTypes
      );
    } catch (err) {
      console.error("Erro ao carregar estruturas:", err);
    }
  }

  // 3. REFACTORIZAÇÃO: Correção do Espaço Vazio (Gap)
  private async createPortStructure(
    docks: any[],
    yards: any[],
    warehouses: any[],
    stsCranes: any[],
    yardCranes: any[],
    approvedVesselVisits: any[],
    vessels: any[],
    vesselTypes: any[]
  ): Promise<void> {

    // --- CONFIGURAÇÕES ---
    const DOCK_WIDTH = 30;
    const DOCK_DEPTH = 10;
    const DOCK_GAP = 5;

    const YARD_WIDTH = 60;
    const YARD_DEPTH = 40;

    const WH_WIDTH = 40;
    const WH_DEPTH = 25;

    const COL_GAP = 15;
    const ROW_GAP = 20;
    const MARGIN_SIDE = 20;
    const ROAD_FRONT = 40;
    const ZONE_GAP = 40;

    const numWH = warehouses.length;
    const numYD = yards.length;

    const colsWH = numWH > 0 ? Math.round(Math.sqrt(numWH)) : 0;
    const colsYD = numYD > 0 ? Math.round(Math.sqrt(numYD)) : 0;

    const finalColsWH = (numWH > 0 && colsWH === 0) ? 1 : colsWH;
    const finalColsYD = (numYD > 0 && colsYD === 0) ? 1 : colsYD;

    const widthBlockWH = finalColsWH > 0 ? finalColsWH * (WH_WIDTH + COL_GAP) - COL_GAP : 0;
    const widthBlockYD = finalColsYD > 0 ? finalColsYD * (YARD_WIDTH + COL_GAP) - COL_GAP : 0;

    let buildingsTotalWidth = widthBlockWH + widthBlockYD;
    if (numWH > 0 && numYD > 0) {
      buildingsTotalWidth += ZONE_GAP;
    }

    const docksTotalWidth = docks.length * (DOCK_WIDTH + DOCK_GAP) - DOCK_GAP;

    let portWidth = Math.max(docksTotalWidth, buildingsTotalWidth) + (MARGIN_SIDE * 2);
    portWidth = Math.ceil(portWidth / 10) * 10;

    const rowsWH = finalColsWH > 0 ? Math.ceil(numWH / finalColsWH) : 0;
    const rowsYD = finalColsYD > 0 ? Math.ceil(numYD / finalColsYD) : 0;

    const depthBlockWH = rowsWH > 0 ? rowsWH * (WH_DEPTH + ROW_GAP) - ROW_GAP : 0;
    const depthBlockYD = rowsYD > 0 ? rowsYD * (YARD_DEPTH + ROW_GAP) - ROW_GAP : 0;

    let maxBuildingDepth = Math.max(depthBlockWH, depthBlockYD);
    let physicalDepth = ROAD_FRONT + maxBuildingDepth + MARGIN_SIDE;

    let squaredDepth = portWidth * 0.6;

    let portDepth = Math.max(physicalDepth, squaredDepth);

    portDepth = Math.max(portDepth, 140);

    portDepth = Math.ceil(portDepth / 10) * 10;

    console.log(`Layout: ${finalColsWH} cols WH | ${finalColsYD} cols YD`);
    console.log(`Port Size: ${portWidth}x${portDepth}`);

    const port = PortBuilder.createPort(portWidth, portDepth);
    port.position.set(0, 0, portDepth / 2);
    this.scene.add(port);

    const startDocksX = -((docks.length * (DOCK_WIDTH + DOCK_GAP)) - DOCK_GAP) / 2 + (DOCK_WIDTH/2);

    docks.forEach((dock, i) => {
      const x = startDocksX + (i * (DOCK_WIDTH + DOCK_GAP));
      const dockGroup = DockBuilder.createDock(
        DOCK_WIDTH, DOCK_DEPTH,
        new THREE.Vector3(x, 0, -(DOCK_DEPTH / 2)),
        dock.name
      );
      DockBuilder.builtDocks.set(dock.id, dockGroup);
      this.scene.add(dockGroup);
      this.docks.push(dockGroup);
    });

    const backLimitZ = portDepth - MARGIN_SIDE;
    const totalContentWidth = buildingsTotalWidth;
    let currentX = -(totalContentWidth / 2);

    if (numWH > 0) {
      const startZ_WH = backLimitZ - depthBlockWH + (WH_DEPTH / 2);
      let whCursorX = currentX + (WH_WIDTH / 2);

      warehouses.forEach((wh, index) => {
        const colIndex = index % finalColsWH;
        const rowIndex = Math.floor(index / finalColsWH);

        const x = whCursorX + (colIndex * (WH_WIDTH + COL_GAP));
        const z = startZ_WH + (rowIndex * (WH_DEPTH + ROW_GAP));

        const whGroup = WarehouseBuilder.createWarehouse(
          WH_WIDTH, WH_DEPTH,
          new THREE.Vector3(x, 0, z),
          wh.id
        );
        this.scene.add(whGroup);
      });

      currentX += widthBlockWH + ZONE_GAP;
    }

    if (numYD > 0) {
      const startZ_YD = backLimitZ - depthBlockYD + (YARD_DEPTH / 2);
      let ydCursorX = currentX + (YARD_WIDTH / 2);

      yards.forEach((yd, index) => {
        const colIndex = index % finalColsYD;
        const rowIndex = Math.floor(index / finalColsYD);

        const x = ydCursorX + (colIndex * (YARD_WIDTH + COL_GAP));
        const z = startZ_YD + (rowIndex * (YARD_DEPTH + ROW_GAP));

        const yardGroup = YardBuilder.createYard(
          YARD_WIDTH, YARD_DEPTH,
          new THREE.Vector3(x, 0, z),
          yd.code
        );
        this.scene.add(yardGroup);

        YardBuilder.lastBuiltYards.set(yd.id, {
          group: yardGroup,
          center: getYardCenter(yardGroup),
          width: YARD_WIDTH, depth: YARD_DEPTH
        });
      });
    }

    stsCranes.forEach(crane => {
      const dock = DockBuilder.builtDocks.get(crane.assignedArea);
      if (!dock) return;
      const c = getDockCenter(dock);
      const craneObj = StsCraneBuilder.createCrane(6, 25, 20, new THREE.Vector3(c.x, 0, 6), crane.id);
      craneObj.rotation.y = Math.PI / 2;
      this.scene.add(craneObj);
    });

    yardCranes.forEach(crane => {
      const info = YardBuilder.lastBuiltYards.get(crane.assignedArea);
      if (!info) return;
      const obj = YardGantryCraneBuilder.createCrane(info.width, info.depth, 18, info.center, crane.id);
      this.scene.add(obj);
    });
    //const vesselVisits = await firstValueFrom(this.adminService.getAllVesselVisitNotifications());
    //console.log(vesselVisits);
    //const approvedVisits = vesselVisits.filter(v => v.status === 'Approved');

    approvedVesselVisits.forEach(visit => {
      // Encontrar o dock pelo id
      const dockInfo = docks.find(d => d.id === visit.assignedDock);
      if (!dockInfo) return;

      const dock = DockBuilder.builtDocks.get(dockInfo.name);
      if (!dock) return;

      const dockCenter = getDockCenter(dock);
      // Encontrar o vessel pelo vesselId
      const vesselData = vessels.find(v => v.id === visit.vesselId);
      if (!vesselData) return;

      const vesselTypeData = vesselTypes.find(vt => vt.id === vesselData.vesselTypeId);
      if (!vesselTypeData) return;

      // posição do vessel ao lado do dock
      const vesselPosition = dockCenter.clone();
      vesselPosition.z -= (DOCK_DEPTH * 2) ; // à frente do dock
      vesselPosition.y = 0; // ao nível da água

      const vessel = VesselBuilder.createVessel({
        maxRows: vesselTypeData.maxRows,
        maxTiers: vesselTypeData.maxTiers,
        maxBays: vesselTypeData.maxBays,
        vesselId: visit.vesselId,
        position: vesselPosition
      });

      this.scene.add(vessel);
    });

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

  private updateCameraMovement(dt: number): void {
    const dir = new THREE.Vector3();
    this.camera.getWorldDirection(dir);

    const right = new THREE.Vector3();
    right.crossVectors(dir, this.camera.up).normalize();

    const speed = this.movementSpeed * dt * 15;

    if (this.movement.forward) this.camera.position.addScaledVector(dir, speed);
    if (this.movement.backward) this.camera.position.addScaledVector(dir, -speed);
    if (this.movement.left) this.camera.position.addScaledVector(right, -speed);
    if (this.movement.right) this.camera.position.addScaledVector(right, speed);

    if (this.movement.up) this.camera.position.y += speed;
    if (this.movement.down) this.camera.position.y -= speed;

    // manter a target sincronizada
    this.controls.target.addScaledVector(dir, 0);
    this.controls.update();
  }


  private createScene(): void {

    this.scene.background = new THREE.Color(0xffffff);

    const ambient = new THREE.AmbientLight(0xffffff, 0.8);
    this.scene.add(ambient);

    const sun = new THREE.DirectionalLight(0xffffff, 0.6);
    sun.position.set(10, 10, 5);
    this.scene.add(sun);

    // ALTERAÇÃO: Chamar o novo método de carregamento
    this.loadPortStructure();

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
    this.updateCameraMovement(dt);
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
    // ALTERAÇÃO: createScene é chamado aqui e internamente chama loadPortStructure
    this.createScene();

    this.renderer = new THREE.WebGLRenderer({
      canvas: this.canvas,
      antialias: true,
      alpha: true
    });

    this.renderer.setPixelRatio(Math.min(window.devicePixelRatio, 2));
    this.renderer.setSize(this.canvas.clientWidth, this.canvas.clientHeight, false);

    // 1) Criar OrbitControls ANTES de usar
    this.controls = new OrbitControls(this.camera, this.canvas);
    this.controls.enableDamping = true;
    this.controls.dampingFactor = 0.05;
    this.controls.screenSpacePanning = false;
    this.controls.minDistance = 5;
    this.controls.maxDistance = 300;
    this.controls.maxPolarAngle = Math.PI / 2;

    // 2) Só agora adicionamos os controlos de teclado
    this.addKeyboardControls();

    // 3) Click p/ animar câmera
    this.canvas.addEventListener('click', (event) => this.onCanvasClick(event));

    // 4) Resize
    this.resizeObserver = new ResizeObserver(() => this.onWindowResize());
    this.resizeObserver.observe(this.canvas);

    // 5) Start animation
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

  private addKeyboardControls(): void {
    window.addEventListener('keydown', (e) => {
      switch (e.key.toLowerCase()) {
        case 'w': this.movement.forward = true; break;
        case 's': this.movement.backward = true; break;
        case 'a': this.movement.left = true; break;
        case 'd': this.movement.right = true; break;
        case ' ': this.movement.up = true; break;
        case 'shift': this.movement.down = true; break;
      }
    });

    window.addEventListener('keyup', (e) => {
      switch (e.key.toLowerCase()) {
        case 'w': this.movement.forward = false; break;
        case 's': this.movement.backward = false; break;
        case 'a': this.movement.left = false; break;
        case 'd': this.movement.right = false; break;
        case ' ': this.movement.up = false; break;
        case 'shift': this.movement.down = false; break;
      }
    });
  }


}
function getYardCenter(yard: THREE.Group): THREE.Vector3 {
  const box = new THREE.Box3().setFromObject(yard);
  const center = new THREE.Vector3();
  box.getCenter(center);
  return center;
}
function getDockCenter(dock: THREE.Group): THREE.Vector3 {
  const box = new THREE.Box3().setFromObject(dock);
  const center = new THREE.Vector3();
  box.getCenter(center);
  return center;
}
function getObjectTopY(object: THREE.Object3D): number {
  const box = new THREE.Box3().setFromObject(object);
  return box.max.y;
}

