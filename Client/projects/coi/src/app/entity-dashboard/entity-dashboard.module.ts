import { NgModule } from '@angular/core';
import { CommonModule } from '@angular/common';
import { EntityDashboardComponent } from './entity-dashboard.component';
import { RouterModule, Routes } from '@angular/router';
import { FormsModule } from '@angular/forms';
import { MatIconModule } from '@angular/material/icon';
import { SharedLibraryModule } from 'projects/shared/src/public-api';
import { EntityDetailsModule } from '../disclosure/entity-details/entity-details.module';
import { SharedComponentModule } from '../shared-components/shared-component.module';
import { SharedModule } from '../shared/shared.module';

const routes: Routes = [{path: '', component: EntityDashboardComponent}];

@NgModule({
  declarations: [
    EntityDashboardComponent
  ],
  imports: [
    CommonModule,
    RouterModule.forChild(routes),
    MatIconModule,
    SharedModule,
    FormsModule,
    SharedComponentModule,
    EntityDetailsModule,
    SharedLibraryModule
  ]
})
export class EntityDashboardModule { }
