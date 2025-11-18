
import * as THREE from 'three';
export class ContainerBuilder {
  static createContainer(is40ft = false): THREE.Mesh {
    const length = is40ft ? 12 : 6;
    const height = 2.5;
    const width = 2.4;

    const geom = new THREE.BoxGeometry(length, height, width);

    const colors = [0xd32f2f, 0x1976d2, 0xfc9803, 0x388e3c, 0x7b1fa2,0xcccccc, 0x009688, 0x558b2f,0x3f3f3f];
    const color = colors[Math.floor(Math.random() * colors.length)];

    const mat = new THREE.MeshStandardMaterial({ color });

    return new THREE.Mesh(geom, mat);
  }
}
