// scene/VesselBuilder.ts
import * as THREE from 'three';

export interface VesselOptions {
  maxRows: number;     // largura (containers por fila)
  maxTiers: number;    // altura (camadas)
  maxBays: number;     // comprimento (quantas colunas ao longo do navio)
  vesselId: string;
  position: THREE.Vector3; // posição onde o centro do navio ficará
}

/**
 * VesselBuilder
 * Cria um ship-container visualmente reconhecível:
 * - hull (casco) com proa (cone) e popa (rampa levemente inclinada)
 * - convés
 * - superestrutura / bridge
 * - containers empilhados organizados por rows x tiers x bays
 *
 * O resultado é um THREE.Group centrado na origem; o método coloca o grupo em `options.position`.
 */
export class VesselBuilder {
  static createVessel(options: VesselOptions): THREE.Group {
    const { maxRows, maxTiers, maxBays, vesselId, position } = options;

    const vessel = new THREE.Group();
    vessel.name = `Vessel_${vesselId}`;

    // ---- parâmetros de dimensão ----
    const CONTAINER_W = 2.5;
    const CONTAINER_H = 2.5;
    const CONTAINER_D = 6;

    const gapX = 0.25;
    const gapY = 0.15;
    const gapZ = 0.25;

    // dimensões do conjunto de containers (usadas para hull)
    const containersWidth = maxRows * (CONTAINER_W + gapX) - gapX;
    const containersLength = maxBays * (CONTAINER_D + gapZ) - gapZ;
    const containersHeight = maxTiers * (CONTAINER_H + gapY) - gapY;

    // hull (base) - um box um pouco maior que os containers
    const hullExtra = 4;
    const hullWidth = Math.max(8, containersWidth + hullExtra);
    const hullLength = Math.max(12, containersLength + hullExtra);
    const hullHeight = 3.5;

    const hullGeom = new THREE.BoxGeometry(hullWidth, hullHeight, hullLength);
    const hullMat = new THREE.MeshStandardMaterial({ color: 0x223355, roughness: 0.7, metalness: 0.2 });
    const hull = new THREE.Mesh(hullGeom, hullMat);
    hull.castShadow = true;
    hull.receiveShadow = true;
    // posiciona o hull ligeiramente acima da água (centro em y = hullHeight/2)
    hull.position.y = hullHeight / 2;
    vessel.add(hull);

    // proa (cone) para dar forma ao nariz do navio (apontada para -Z)
    const bowLength = Math.min(hullLength * 0.3, 14);
    const bowGeom = new THREE.CylinderGeometry(0, hullWidth * 0.45, bowLength, 16);
    const bowMat = hullMat.clone();
    const bow = new THREE.Mesh(bowGeom, bowMat);
    bow.rotation.x = Math.PI / 2; // alinhamos o eixo do cone ao eixo Z
    // posicionar na frente (Z negativo)
    bow.position.z = -hullLength / 2 - bowLength / 2 + 0.5;
    bow.position.y = hull.position.y;
    vessel.add(bow);

    // popa: leve rampa / caixa arredondada para trás
    const sternGeom = new THREE.BoxGeometry(hullWidth * 0.9, hullHeight * 0.8, hullLength * 0.12);
    const stern = new THREE.Mesh(sternGeom, hullMat);
    stern.position.z = hullLength / 2 - (sternGeom.parameters.depth || hullLength * 0.12) / 2 - 0.2;
    stern.position.y = hull.position.y;
    vessel.add(stern);

    // convés - uma placa fina sobre o hull
    const deckGeom = new THREE.BoxGeometry(hullWidth * 0.98, 0.4, hullLength * 0.98);
    const deckMat = new THREE.MeshStandardMaterial({ color: 0x556677, roughness: 0.6 });
    const deck = new THREE.Mesh(deckGeom, deckMat);
    deck.position.y = hull.position.y + hullHeight / 2 + 0.2;
    vessel.add(deck);

    // bridge / superestrutura (uma pequena torre próxima da popa)
    const bridgeWidth = Math.min(10, hullWidth * 0.4);
    const bridgeDepth = Math.min(10, hullLength * 0.16);
    const bridgeHeight = 6 + Math.min(3, Math.floor(maxTiers / 2));

    const bridgeGeom = new THREE.BoxGeometry(bridgeWidth, bridgeHeight, bridgeDepth);
    const bridgeMat = new THREE.MeshStandardMaterial({ color: 0xdedede, roughness: 0.5, metalness: 0.1 });
    const bridge = new THREE.Mesh(bridgeGeom, bridgeMat);
    bridge.position.y = deck.position.y + bridgeHeight / 2 + 0.2;
    bridge.position.z = hullLength / 2 - bridgeDepth / 2 - 1.0; // mais perto da popa
    bridge.position.x = 0;
    vessel.add(bridge);

    // detalhes na bridge: janelas (simples planos escuros)
    const windowGeom = new THREE.PlaneGeometry(bridgeWidth * 0.9, 1.0);
    const windowMat = new THREE.MeshStandardMaterial({ color: 0x111111, side: THREE.DoubleSide });
    const windows = new THREE.Mesh(windowGeom, windowMat);
    windows.position.y = bridge.position.y + 1.0;
    windows.position.z = bridge.position.z + bridgeDepth / 2 + 0.01;
    vessel.add(windows);

    // ------- containers: posicionar sobre o deck/hull com offset para ficar centralizado -------
    const containerGeom = new THREE.BoxGeometry(CONTAINER_W, CONTAINER_H, CONTAINER_D);
    const containerColors = [0xff6b6b, 0x6bffb0, 0x6b9bff, 0xffe26b, 0xff6bf0, 0x6bfff1];

    // calcular origem (canto superior-esquerdo relativo ao centro do hull)
    const startX = - ( (maxRows - 1) * (CONTAINER_W + gapX) ) / 2;
    const startZ = - ( (maxBays - 1) * (CONTAINER_D + gapZ) ) / 2;
    const baseY = deck.position.y + 0.2 + CONTAINER_H / 2; // containers apoiados no convés

    for (let r = 0; r < maxRows; r++) {
      for (let b = 0; b < maxBays; b++) {
        for (let t = 0; t < maxTiers; t++) {
          const mat = new THREE.MeshStandardMaterial({
            color: containerColors[(r + b + t) % containerColors.length],
            roughness: 0.6,
            metalness: 0.2
          });
          const c = new THREE.Mesh(containerGeom, mat);
          c.castShadow = true;
          c.receiveShadow = false;

          c.position.x = startX + r * (CONTAINER_W + gapX);
          c.position.z = startZ + b * (CONTAINER_D + gapZ);
          c.position.y = baseY + t * (CONTAINER_H + gapY);

          vessel.add(c);
        }
      }
    }

    // ---- Centralizar tudo no centro do grupo (origem) ----
    // Em vez de mover o group, movemos as children para que o centro geométrico fique na origem
    const box = new THREE.Box3().setFromObject(vessel);
    const center = new THREE.Vector3();
    box.getCenter(center);

    vessel.children.forEach((child) => {
      // ajusta a posição de cada child para centralizar o conjunto
      child.position.sub(center);
    });

    // agora colocamos o grupo na posição desejada (o centro do navio fica em options.position)
    vessel.position.copy(position);

    // orientar proa para -Z (á água nas tuas docks estava em Z negativo)
    vessel.rotation.y = Math.PI / 2;

    // tag informativa (útil para debugging)
    // const label = new THREE.Mesh(new THREE.BoxGeometry(0.1,0.1,0.1), new THREE.MeshBasicMaterial({color:0xff0000}));
    // label.position.set(0, 0.5, 0);
    // vessel.add(label);

    return vessel;
  }
}
