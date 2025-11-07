import { Routes } from '@angular/router';
import { Cube } from './cube/cube';
import { App } from './app';
export const routes: Routes = [
// Redirect to the cube component on app load
  { path: '', component: App },
// Route for the cube component
  { path: 'cube', component: Cube },
];
