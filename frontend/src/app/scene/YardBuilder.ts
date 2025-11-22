import * as THREE from 'three';
import { ContainerBuilder } from './ContainerBuilder';

export class YardBuilder {
  // Guarda o centro da última yard criada
  static lastYardCenter: THREE.Vector3 = new THREE.Vector3();
  static lastYardSize: { width: number, depth: number } = { width: 0, depth: 0 };

  static createYard(width: number, depth: number, position: THREE.Vector3, id: string): THREE.Group {

    const group = new THREE.Group();
    // ---------------------------------------
    // 🟦 TEXTURA DO CHÃO
    // ---------------------------------------
    const textureLoader = new THREE.TextureLoader();

    const groundTexture = textureLoader.load('assets/textures/yard/yard_ground.jpg');
    groundTexture.wrapS = THREE.RepeatWrapping;
    groundTexture.wrapT = THREE.RepeatWrapping;

    // repetir conforme tamanho do pátio
    groundTexture.repeat.set(width / 20, depth / 20);

    groundTexture.anisotropy = 16;

    // ---------------------------------------
    // 🟫 CHÃO
    // ---------------------------------------
    const groundHeight = 1.3;
    const groundGeom = new THREE.BoxGeometry(width, groundHeight, depth);

    const groundMat = new THREE.MeshStandardMaterial({
      map: groundTexture,
      roughness: 0.9,
      metalness: 0.1,
    });

    const ground = new THREE.Mesh(groundGeom, groundMat);
    const groundTop = groundHeight / 2;

    ground.position.y = groundTop;
    group.add(ground);

    // ---- Configuração de grelha ----
    // ---- Medir altura real do contentor, incluindo offsets internos ----
    const tempContainer = ContainerBuilder.createContainer(true);
    const tempBox = new THREE.Box3().setFromObject(tempContainer);
    const containerMinY = tempBox.min.y;   // fundo real do contentor
    const containerMaxY = tempBox.max.y;   // topo real do contentor
    const containerHeight = containerMaxY - containerMinY;


    const containerLength = 6;
    const spacing = 7;

    const cols = Math.floor(width / spacing);
    const rows = Math.floor(depth / spacing);

    const maxStack = 2;  // 0,1,2 → até 3 níveis
    let minX = Infinity, maxX = -Infinity;
    let minZ = Infinity, maxZ = -Infinity;

    for (let x = 0; x < cols; x++) {
      for (let z = 0; z < rows; z++) {

        const stackHeight = Math.floor(Math.random() * (maxStack + 1));

        for (let h = 0; h <= stackHeight; h++) {

          const container = ContainerBuilder.createContainer(true);

          const posX = -width / 2 + x * spacing + spacing / 2;
          const posZ = -depth / 2 + z * spacing + spacing / 2;

          // 👉 AQUI corrigimos: container fica exatamente no topo do chão.
          const posY =
            groundTop +                // topo da plataforma
            (containerHeight / 2) -    // centro do contentor
            containerMinY +            // corrigir offset do modelo
            h * containerHeight;       // stack



          container.position.set(posX, posY, posZ);

          group.add(container);

          // atualizar extremos
          if (posX < minX) minX = posX;
          if (posX > maxX) maxX = posX;
          if (posZ < minZ) minZ = posZ;
          if (posZ > maxZ) maxZ = posZ;
        }
      }
    }

    // ---- Posicionamento final ----
    group.position.copy(position);
    group.position.y = 0;

    group.name = `Yard_${id}`;

    // ---- Calcular centro da yard usando apenas rows e cols ----
    //const centerX = -width / 2 + (cols * spacing) / 2;
    //const centerZ = -depth / 2 + (rows * spacing) / 2;
    //YardBuilder.lastYardCenter = new THREE.Vector3(centerX, 0, centerZ).add(position);
    YardBuilder.lastYardCenter = position;
    // ---- Calcular tamanho real da yard (X = largura, Z = profundidade) ----
    YardBuilder.lastYardSize = { width: maxX - minX, depth: maxZ - minZ };

    YardBuilder.lastBuiltYards.set(id, {
      group: group,
      center: YardBuilder.lastYardCenter.clone(),
      width,
      depth
    });

    return group;
  }
  static lastBuiltYards: Map<string, {
    group: THREE.Group,
    center: THREE.Vector3,
    width: number,
    depth: number
  }> = new Map();




  /*
  static calculateCenter(yard: THREE.Group): THREE.Vector3 {
    let minX = Infinity, maxX = -Infinity;
    let minZ = Infinity, maxZ = -Infinity;

    yard.children.forEach(obj => {
      if (obj.name.startsWith('Container')) {
        const worldPos = new THREE.Vector3();
        obj.getWorldPosition(worldPos);
        if (worldPos.x < minX) minX = worldPos.x;
        if (worldPos.x > maxX) maxX = worldPos.x;
        if (worldPos.z < minZ) minZ = worldPos.z;
        if (worldPos.z > maxZ) maxZ = worldPos.z;
      }
    });

    return new THREE.Vector3(
      (minX + maxX) / 2,
      0,
      (minZ + maxZ) / 2
    );
  }
  */
}
