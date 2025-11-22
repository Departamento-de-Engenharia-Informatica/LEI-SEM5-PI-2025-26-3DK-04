// ContainerBuilder.ts
import * as THREE from 'three';

export type ContainerColor = "blue" | "red" | "green" | "black" | "white" | "yellow";

export class ContainerBuilder {

  private static textureLoader = new THREE.TextureLoader();

  static getContainerMaterials(color: ContainerColor): THREE.MeshStandardMaterial[] {

    const basePath = `assets/textures/container/${color}`;

    const texRight  = this.textureLoader.load(`${basePath}_lateral_side.jpg`);
    const texLeft   = this.textureLoader.load(`${basePath}_lateral_side.jpg`);
    const texTop    = this.textureLoader.load(`${basePath}_lateral_side.jpg`);
    const texBottom = this.textureLoader.load(`${basePath}_lateral_side.jpg`);
    const texFront  = this.textureLoader.load(`${basePath}_back_side.jpg`);
    const texDoor   = this.textureLoader.load(`${basePath}_door_side.jpg`);

    return [
      new THREE.MeshStandardMaterial({ map: texRight }),
      new THREE.MeshStandardMaterial({ map: texLeft }),
      new THREE.MeshStandardMaterial({ map: texTop }),
      new THREE.MeshStandardMaterial({ map: texBottom }),
      new THREE.MeshStandardMaterial({ map: texFront }),
      new THREE.MeshStandardMaterial({ map: texDoor })
    ];
  }


  static createContainer(randomColor: boolean = true): THREE.Mesh {
    const geom = new THREE.BoxGeometry(2.5, 2.6, 6);

    const colors: ContainerColor[] =
      ["blue", "red", "green", "black", "white", "yellow"];

    const chosen: ContainerColor = randomColor
      ? colors[Math.floor(Math.random() * colors.length)]
      : "red";

    const mats = this.getContainerMaterials(chosen);

    const mesh = new THREE.Mesh(geom, mats);
    mesh.name = `Container_${chosen}`;
    mesh.castShadow = true;
    mesh.receiveShadow = true;
    return mesh;
  }
}
