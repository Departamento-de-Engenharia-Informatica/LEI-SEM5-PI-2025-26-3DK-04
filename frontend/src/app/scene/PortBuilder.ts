import * as THREE from 'three';

export class PortBuilder {

  static createPort(width: number, height: number, color: number = 0xCCCCCC): THREE.Mesh {
    const geometry = new THREE.BoxGeometry(width, 1, height);
    const material = new THREE.MeshStandardMaterial({ color });

    const port = new THREE.Mesh(geometry, material);
    port.name = "PortArea";

    // O porto fica sempre no nível 0
    port.position.set(0, -0.5, 0);

    return port;
  }
}
