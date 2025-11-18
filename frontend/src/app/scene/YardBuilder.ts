import * as THREE from 'three';
import { ContainerBuilder } from './ContainerBuilder';

export class YardBuilder {
  // Guarda o centro da última yard criada
  static lastYardCenter: THREE.Vector3 = new THREE.Vector3();
  static lastYardSize: { width: number, depth: number } = { width: 0, depth: 0 };
  static createYard(width: number, depth: number, position: THREE.Vector3, id: number): THREE.Group {

    const group = new THREE.Group();

    // ---- Chão do pátio ----
    const groundGeom = new THREE.BoxGeometry(width, 0.3, depth);
    const groundMat = new THREE.MeshStandardMaterial({ color: 0xb8b8a8 });
    const ground = new THREE.Mesh(groundGeom, groundMat);
    ground.position.y = 0.15;
    group.add(ground);

    // ---- Configuração de grelha ----
    const containerHeight = 2.6;
    const containerLength = 6;          // 20ft
    const spacing = 7;                  // 6m + 1m corredor entre stacks

    const cols = Math.floor(width / spacing);
    const rows = Math.floor(depth / spacing);

    const maxStack = 2;  // 0,1,2 → até 3 níveis
    let minX = Infinity, maxX = -Infinity;
    let minZ = Infinity, maxZ = -Infinity;
    for (let x = 0; x < cols; x++) {
      for (let z = 0; z < rows; z++) {

        const stackHeight = Math.floor(Math.random() * (maxStack + 1));

        for (let h = 0; h <= stackHeight; h++) {

          const container = ContainerBuilder.createContainer(false);
          const posX = -width / 2 + x * spacing + spacing / 2;
          const posY = containerHeight / 2 + h * containerHeight;
          const posZ = -depth / 2 + z * spacing + spacing / 2;
          container.position.set(posX, posY, posZ
          );

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
    const centerX = -width / 2 + (cols * spacing) / 2;
    const centerZ = -depth / 2 + (rows * spacing) / 2;
    YardBuilder.lastYardCenter = new THREE.Vector3(centerX, 0, centerZ).add(position);

    // ---- Calcular tamanho real da yard (X = largura, Z = profundidade) ----
    YardBuilder.lastYardSize = { width: maxX - minX, depth: maxZ - minZ };

    return group;
  }
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

