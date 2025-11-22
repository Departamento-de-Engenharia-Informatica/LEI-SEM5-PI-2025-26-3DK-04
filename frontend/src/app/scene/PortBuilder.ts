import * as THREE from 'three';

export class PortBuilder {
  private static textureLoader = new THREE.TextureLoader();

  static createPort(width: number, height: number, texturePath: string = 'assets/textures/port/port_ground.jpg'): THREE.Mesh {
    const geometry = new THREE.BoxGeometry(width, 1, height);

    // carregar textura
    const texture = this.textureLoader.load(texturePath);
    texture.wrapS = THREE.RepeatWrapping;
    texture.wrapT = THREE.RepeatWrapping;

    // repetir textura conforme o tamanho do porto
    //texture.repeat.set(width / 5, height / 5); // ajustar 5 para escala desejada

    const material = new THREE.MeshStandardMaterial({ map: texture });

    const port = new THREE.Mesh(geometry, material);
    port.name = "PortArea";

    // O porto fica sempre no nível 0
    port.position.set(0, -0.5, 0);

    return port;
  }
}
