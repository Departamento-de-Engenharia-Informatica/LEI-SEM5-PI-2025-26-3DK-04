import { Component, AfterViewInit, ElementRef, ViewChild, Input, OnDestroy } from '@angular/core';
import * as THREE from 'three';
import { OrbitControls } from 'three/examples/jsm/controls/OrbitControls.js';
import { firstValueFrom } from 'rxjs';
import { DockBuilder } from '../scene/DockBuilder';
import { PortBuilder } from '../scene/PortBuilder';
import { AdminService } from '../admin/admin.service';

// NOVOS IMPORTS
import { WarehouseBuilder } from '../scene/WarehouseBuilder';
import { YardBuilder } from '../scene/YardBuilder';
import { YardGantryCraneBuilder, YardCraneType } from '../scene/YardGantryCraneBuilder';
import { StsCraneBuilder } from '../scene/StsCraneBuilder';
import { VesselBuilder } from '../scene/VesselBuilder';

// -------------------------------------------------------------
// --- NOVAS INTERFACES PARA O JSON DO LAYOUT (Backend) ---
// -------------------------------------------------------------

interface PortElement {
  type: 'dock' | 'storage_area';
  subtype?: 'yard' | 'warehouse';
  id_placeholder: string;
  position: { x: number; z: number; };
  size: { width: number; depth: number; };
}

export interface PortLayout {
  port_dimensions: { width: number; depth: number; };
  elements: PortElement[];
}

@Component({
  selector: 'app-dock-view',
  standalone: true,
  imports: [],
  templateUrl: './dock-view.html',
  styleUrl: './dock-view.scss',
})
export class DockView implements AfterViewInit, OnDestroy {
  @ViewChild('myCanvas') private canvasRef!: ElementRef;

  // ... (Propriedades de Stage e Variáveis de Three.js mantidas) ...
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
  // -------------------------------------------------------------------

  // 1. INJEÇÃO DO SERVIÇO
  constructor(private adminService: AdminService) {}


  // 2. MÉTODO PARA CARREGAR DADOS ASINCRONAMENTE (Inclui o novo endpoint)
  private async loadPortStructure(): Promise<void> {
    try {
      // NOVO: Obter o layout pré-calculado (JSON)
      const layout : PortLayout = await firstValueFrom(this.adminService.getPortLayout());
      const docks = await firstValueFrom(this.adminService.getAllDocks());
      const storageAreas = await firstValueFrom(this.adminService.getAllStorageAreas());
      const yards = storageAreas.filter(a => a.storageAreaType === "Yard");
      // Dados complementares para Cranes e Vessels
      const resources = await firstValueFrom(this.adminService.getAllPhysicalResources());
      const approvedVesselVisits = await firstValueFrom(this.adminService.getApprovedVesselVisitNotifications());
      const vessels = await firstValueFrom(this.adminService.getVessels());
      const vesselTypes = await firstValueFrom(this.adminService.getVesselTypes());

      const stsCranes = resources.filter(r => r.type === "STS_CRANE");
      const yardCranes = resources.filter(r => r.type === "YARD_CRANE");

      // Mapeamento dos Placeholders para os IDs reais e dados de layout
      const docksData = layout.elements.filter(e => e.type === 'dock');
      const yardsData = layout.elements.filter(e => e.subtype === 'yard');
      const warehousesData = layout.elements.filter(e => e.subtype === 'warehouse');

      // O Map será usado para passar dados (tamanho/posição) para Cranes/Vessels que usam o ID real
      const dockMap = new Map<string, PortElement>();
      docksData.forEach(d => {
        // Usa o ID real (ex: 'DOCK001') como chave do Map
        const dockId = d.id_placeholder;
        dockMap.set(dockId, d as PortElement);
      });

      const yardMap = new Map<string, PortElement>();
      yardsData.forEach(y => {
        // Usa o ID real (ex: 'YARD001') como chave do Map
        const yardId = y.id_placeholder;
        yardMap.set(yardId, y as PortElement);
      });

      await this.createPortStructure(
        layout.port_dimensions,
        dockMap,
        yardMap,
        docks,
        yards,
        warehousesData,
        stsCranes,
        yardCranes,
        approvedVesselVisits,
        vessels,
        vesselTypes
      );
    } catch (err) {
      console.error("Erro ao carregar estruturas:", err);
    }
  }

  private async createPortStructure(
    portDimensions: { width: number, depth: number },
    dockMap: Map<string, PortElement>,
    yardMap: Map<string, PortElement>,
    docks: any[],
    yards: any[],
    warehouses: PortElement[],
    stsCranes: any[],
    yardCranes: any[],
    approvedVesselVisits: any[],
    vessels: any[],
    vesselTypes: any[]
  ): Promise<void> {

    const DOCK_DEPTH_SCALE = 1;
    const CRANE_Z_POSITION = -65.5;
    const VESSEL_DOCK_OFFSET = 20;

    const portWidth = portDimensions.width;
    const portDepth = portDimensions.depth;

    console.log(`Port Size: ${portWidth}x${portDepth} (Calculado pelo Backend)`);

    const port = PortBuilder.createPort(portWidth, portDepth);

    port.position.set(0, 0, 0);
    this.scene.add(port);

    Array.from(dockMap.entries()).forEach(([dockId, data]) => {
      let dockName: string = "";

      docks.forEach((dock) => {
          if (dock.id === dockId) {
              dockName = dock.name;
          }
      });
      const DOCK_LEN = data.size.width;
      const DOCK_DEP = data.size.depth * DOCK_DEPTH_SCALE;

      // NOVO: Usar as coordenadas X e Z diretamente
      const x = data.position.x;
      const z = data.position.z;
      let z2: number = z;
      const dockGroup = DockBuilder.createDock(
        DOCK_LEN, DOCK_DEP,
        new THREE.Vector3(x, 0, z),
        dockName // Nome de Exemplo
      );
      DockBuilder.builtDocks.set(dockId, {
        group :dockGroup,
        z : z2
      });
      this.scene.add(dockGroup);
      this.docks.push(dockGroup);
    });

    // --- WAREHOUSES ---
    warehouses.forEach(wh => {
      const whId = wh.id_placeholder;
      const currentWH_WIDTH = wh.size.width;
      const currentWH_DEPTH = wh.size.depth;

      // NOVO: Usar as coordenadas X e Z diretamente
      const x = wh.position.x;
      const z = wh.position.z;

      const whGroup = WarehouseBuilder.createWarehouse(
        currentWH_WIDTH, currentWH_DEPTH,
        new THREE.Vector3(x, 0, z),
        whId
      );
      this.scene.add(whGroup);
    });

    // --- YARDS ---
    Array.from(yardMap.entries()).forEach(([yardId, data]) => {
      let yardName: string = "";

      yards.forEach((yard) => {
        if (yard.id === yardId) {
          yardName = yard.code;
        }
      });
      console.log("YARD INFO:",yardName);

      const currentYARD_WIDTH = data.size.width;
      const currentYARD_DEPTH = data.size.depth;

      // NOVO: Usar as coordenadas X e Z diretamente
      const x = data.position.x;
      const z = data.position.z;

      const yardGroup = YardBuilder.createYard(
        currentYARD_WIDTH, currentYARD_DEPTH,
        new THREE.Vector3(x, 0, z),
        yardName // Code/Name de Exemplo
      );
      this.scene.add(yardGroup);

      // Armazenar info para posicionamento de guindastes
      YardBuilder.lastBuiltYards.set(yardId, {
        group: yardGroup,
        center: getYardCenter(yardGroup),
        width: currentYARD_WIDTH, depth: currentYARD_DEPTH
      });
    });


    // -----------------------------------------------------------
    // 2. CRANES e VESSELS (Mantido)
    // -----------------------------------------------------------

    // --- STS CRANES ---
    stsCranes.forEach(crane => {
      const dock = DockBuilder.builtDocks.get(crane.assignedArea);
      console.log("CRANE DOCK:", crane.assignedArea, dock);
      if (!dock) return;
      const c = getDockCenter(dock.group);
      let cranePosition:number =    CRANE_Z_POSITION + (-dock.z);
      const craneObj = StsCraneBuilder.createCrane(
        6, 25, 20,
        new THREE.Vector3(c.x, 0, dock.z + cranePosition),
        crane.id
      );
      craneObj.rotation.y = Math.PI / 2;
      this.scene.add(craneObj);
    });

    // --- YARD CRANES ---
    yardCranes.forEach(crane => {
      const info = YardBuilder.lastBuiltYards.get(crane.assignedArea);
      console.log("YARD CRANE INFO:", crane.assignedArea, info);
      if (!info) return;
      const obj = YardGantryCraneBuilder.createCrane(info.width, info.depth, 18, info.center, crane.id);
      this.scene.add(obj);
    });

    // --- VESSELS ---
    approvedVesselVisits.forEach(visit => {
      // Usa o ID real
      const dockLayoutElement = dockMap.get(visit.assignedDock);
      const dock = DockBuilder.builtDocks.get(visit.assignedDock);
      console.log("VESSEL VISIT:", visit.assignedDock, dockLayoutElement, dock);

      if (!dock || !dockLayoutElement) return;

      const dockCenter = getDockCenter(dock.group);

      // Obtém a profundidade real do dock a partir dos dados do layout (JSON)
      const DOCK_DEP_REAL = dockLayoutElement.size.depth * DOCK_DEPTH_SCALE;

      const vesselData = vessels.find(v => v.id === visit.vesselId);
      if (!vesselData) return;

      const vesselTypeData = vesselTypes.find(vt => vt.id === vesselData.vesselTypeId);
      if (!vesselTypeData) return;

      const vesselPosition = dockCenter.clone();
      // Posição da embarcação: Z do dock - metade da profundidade do dock - offset
      vesselPosition.z -= (DOCK_DEP_REAL / 2) + VESSEL_DOCK_OFFSET;
      vesselPosition.y = 0;

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

  // ... (Outros métodos como onCanvasClick, highlightDock, animateScene, etc., mantidos) ...

  private onCanvasClick(event: MouseEvent): void {
    const rect = this.canvas.getBoundingClientRect();
    this.mouse.x = ((event.clientX - rect.left) / rect.width) * 2 - 1;
    this.mouse.y = -((event.clientY - rect.top) / rect.height) * 2 + 1;

    this.raycaster.setFromCamera(this.mouse, this.camera);
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
    this.renderer = new THREE.WebGLRenderer({
      canvas: this.canvas,
      antialias: true,
      alpha: true
    });
    this.renderer.setPixelRatio(Math.min(window.devicePixelRatio, 2));
    this.renderer.setSize(this.canvas.clientWidth, this.canvas.clientHeight, false);

// --- Setup lighting ---
    this.setupLighting();


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
  private setupLighting(): void {
    // --- Luz Ambiente ---
    const ambient = new THREE.AmbientLight(0xffffff, 0.6); // intensidade moderada
    this.scene.add(ambient);

    // --- Luz Direcional (Sol) ---
    const sun = new THREE.DirectionalLight(0xffffff, 0.8);
    sun.position.set(50, 100, 50); // posição "alta" para criar sombras suaves
    sun.castShadow = true;

    // Configuração de sombras
    sun.shadow.mapSize.width = 2048;   // resolução de sombra (não muito alta p/ performance)
    sun.shadow.mapSize.height = 2048;
    sun.shadow.camera.near = 1;
    sun.shadow.camera.far = 500;
    sun.shadow.camera.left = -100;
    sun.shadow.camera.right = 100;
    sun.shadow.camera.top = 100;
    sun.shadow.camera.bottom = -100;
    sun.shadow.bias = -0.001; // reduz artefatos de shadow acne

    this.scene.add(sun);

    // --- Luz do Hemisfério (opcional, para suavizar sombras) ---
    const hemi = new THREE.HemisphereLight(0xddeeff, 0x444422, 0.4);
    // céu azul claro, chão levemente marrom, intensidade baixa
    this.scene.add(hemi);

    // --- Ativar sombras para o renderer ---
    this.renderer.shadowMap.enabled = true;
    this.renderer.shadowMap.type = THREE.PCFSoftShadowMap; // sombras suaves
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
// Funções auxiliares mantidas
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
