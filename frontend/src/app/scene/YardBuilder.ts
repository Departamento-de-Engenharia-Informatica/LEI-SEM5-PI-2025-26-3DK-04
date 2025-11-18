import * as THREE from 'three';
import { ContainerBuilder } from './ContainerBuilder';

export class YardBuilder {

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

    for (let x = 0; x < cols; x++) {
      for (let z = 0; z < rows; z++) {

        const stackHeight = Math.floor(Math.random() * (maxStack + 1));

        for (let h = 0; h <= stackHeight; h++) {

          const container = ContainerBuilder.createContainer(false);

          container.position.set(
            -width / 2 + x * spacing + spacing / 2,
            containerHeight / 2 + h * containerHeight,
            -depth / 2 + z * spacing + spacing / 2
          );

          group.add(container);
        }
      }
    }

    // ---- Posicionamento final ----
    group.position.copy(position);
    group.position.y = 0;

    group.name = `Yard_${id}`;

    return group;
  }
}
