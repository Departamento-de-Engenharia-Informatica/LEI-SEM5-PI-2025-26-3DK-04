import * as THREE from 'three';

export class DockBuilder {

  static createDock(
    width: number,
    depth: number,
    position: THREE.Vector3,
    id: number
  ): THREE.Mesh {

    const geometry = new THREE.BoxGeometry(width, 0.8, depth);

    const material = new THREE.MeshStandardMaterial({
      color: 0x6e6e6e,   // cimento húmido
      roughness: 0.8,
      metalness: 0.1
    });

    const dock = new THREE.Mesh(geometry, material);

    dock.position.copy(position);
    dock.position.y = 0.4; // ligeiramente acima da água
    dock.name = `Dock_${id}`;
    dock.userData = { dockId: id };

    return dock;
  }
}
