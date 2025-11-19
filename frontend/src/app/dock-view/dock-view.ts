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
      // Obter todas as Docas (Docks)
      const docks = await this.adminService.getAllDocks().toPromise();
      const numDocks = docks ? docks.length : 0;

      // Obter todas as Áreas de Armazenamento (Storage Areas)
      const storageAreas = await firstValueFrom(this.adminService.getAllStorageAreas());
      console.log("StorageAreas recebidas:", storageAreas);

      // Inicializa contadores
      let numYards = 0;
      let numWarehouses = 0;


      if (storageAreas) {
        numYards = storageAreas.filter(area => area.storageAreaType === 'Yard').length;
        numWarehouses = storageAreas.filter(area => area.storageAreaType === 'Warehouse').length;
      }

      console.log(`Pátios (Yard, Tipo 0) encontrados: ${numYards}`);
      console.log(`Armazéns (Warehouse, Tipo 1) encontrados: ${numWarehouses}`);
      // Criar a estrutura com base nos números obtidos
      this.createPortStructure(numDocks, numYards, numWarehouses);

      // Código para Gruas (Cranes) - Mantido à parte, mas pode ser integrado aqui
      // Se precisar de criar gruas em função das docas, o código deve vir aqui
      const dockOffsetZ = -75; // Valor de offset Z hardcoded anteriormente

      // Exemplo de como as gruas (STS Cranes) estariam a ser criadas
      // Se não for para mexer, mantenha o código original abaixo, senão crie um loop aqui
      // Se as gruas estiverem 1:1 com as docas, use numDocks

      /*
      // AQUI PODE USAR UM LOOP, caso as gruas sejam 1:1 com as docas
      for(let i = 0; i < numDocks; i++){
          // ... lógica para posicionar gruas baseada na posição da Doca i
      }
      */

      // O código original (hardcoded) para as gruas é mantido no final de createPortStructure

    } catch (error) {
      console.error('Erro ao carregar dados do porto:', error);
      // Opcional: Chamar createPortStructure(0, 0, 0) para mostrar um porto vazio
      this.createPortStructure(0, 0, 0);
    }
  }


  // 3. REFACTORIZAÇÃO DE createPortStructure
  // O método passa a receber o número de estruturas como argumento
  private createPortStructure(numDocks: number, numYards: number, numWarehouses: number): void {

    const port = PortBuilder.createPort(200, 150);
    this.scene.add(port);

    // Variáveis de layout e dimensões padrão (Hardcoded)
    const DOCK_WIDTH = 30;
    const DOCK_DEPTH = 10;
    const DOCK_SPACING_X = 5;

    const YARD_WIDTH = 60;
    const YARD_DEPTH = 40;
    const YARD_SPACING_X = 10;

    const WAREHOUSE_WIDTH = 40;
    const WAREHOUSE_DEPTH = 25;
    const WAREHOUSE_SPACING_X = 10;

    // --- 1. ZONA DAS DOCAS (Z = -70) ---

    const DOCK_Z = -70;
    const DOCK_Y = 0.5;

    // Cálculo da largura total das docas para centralização
    const totalDockWidth = (DOCK_WIDTH + DOCK_SPACING_X) * numDocks - DOCK_SPACING_X;
    let currentXOffset = -totalDockWidth / 2; // Ponto de partida X

    for (let i = 0; i < numDocks; i++) {

      const dockX = currentXOffset + DOCK_WIDTH / 2;

      const dock = DockBuilder.createDock(
        DOCK_WIDTH,
        DOCK_DEPTH,
        new THREE.Vector3(dockX, DOCK_Y, DOCK_Z),
        i + 1
      );
      this.scene.add(dock);
      this.docks.push(dock);

      currentXOffset += DOCK_WIDTH + DOCK_SPACING_X;
    }


    // --- 2. ZONA DOS PÁTIOS (Z = 30) ---

    const YARD_Z = 30;

    // Novo ponto de partida X para os pátios (centralização)
    const totalYardWidth = (YARD_WIDTH + YARD_SPACING_X) * numYards - YARD_SPACING_X;
    currentXOffset = -totalYardWidth / 2;

    for (let i = 0; i < numYards; i++) {

      const yardX = currentXOffset + YARD_WIDTH / 2;

      const yard = YardBuilder.createYard(
        YARD_WIDTH,
        YARD_DEPTH,
        new THREE.Vector3(yardX, 0, YARD_Z),
        i + 1
      );
      this.scene.add(yard);

      currentXOffset += YARD_WIDTH + YARD_SPACING_X;
    }


    // --- 3. ZONA DOS ARMAZÉNS (Z = 70) ---

    const WAREHOUSE_Z = 70;

    // Novo ponto de partida X para os armazéns (centralização)
    const totalWarehouseWidth = (WAREHOUSE_WIDTH + WAREHOUSE_SPACING_X) * numWarehouses - WAREHOUSE_SPACING_X;
    currentXOffset = -totalWarehouseWidth / 2;

    for (let i = 0; i < numWarehouses; i++) {

      const warehouseX = currentXOffset + WAREHOUSE_WIDTH / 2;

      const warehouse = WarehouseBuilder.createWarehouse(
        WAREHOUSE_WIDTH,
        WAREHOUSE_DEPTH,
        new THREE.Vector3(warehouseX, 0, WAREHOUSE_Z),
        i + 1
      );
      this.scene.add(warehouse);

      currentXOffset += WAREHOUSE_WIDTH + WAREHOUSE_SPACING_X;
    }

    // === STS Cranes (HARDCODED) ===
    // Mantido o código original das gruas aqui, conforme pedido
    const dockOffsetZ = -75;

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

    const yardCrane = YardGantryCraneBuilder.createCrane(
      YardBuilder.lastYardSize.width,   // largura real da yard (X) → influencia perna lateral, cabina se quiseres
      YardBuilder.lastYardSize.depth,   // profundidade real da yard (Z) → comprimento do girder
      18,                               // altura das pernas
      YardBuilder.lastYardCenter,       // centro real da yard
      1                                 // id da grua
    );

    this.scene.add(yardCrane);

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
