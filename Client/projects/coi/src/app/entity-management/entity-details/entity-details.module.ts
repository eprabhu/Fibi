import { NgModule } from '@angular/core';
import { CommonModule } from '@angular/common';
import { EntityDetailsComponent } from './entity-details.component';
import { RouterModule, Routes } from '@angular/router';
import { AddEntityDetailsComponent } from './add-entity-details/add-entity-details.component';
import { MatIconModule } from '@angular/material/icon';

const routes: Routes = [{ path: '', component:EntityDetailsComponent }]

@NgModule({
  imports: [
    CommonModule,
    RouterModule.forChild(routes),
    MatIconModule
  ],
  declarations: [
    EntityDetailsComponent,
    AddEntityDetailsComponent,

  ]
})
export class EntityDetailsModule { }
