import { NgModule } from '@angular/core';
import { CommonModule } from '@angular/common';
import { EntityManagementComponent } from './entity-management.component';
import { RouterModule, Routes } from "@angular/router";
import { EntityListComponent } from './entity-list/entity-list.component';
import { ViewEntityDetailsComponent } from './view-entity-details/view-entity-details.component';
import { MatIconModule } from '@angular/material/icon';
import { EntityDetailsListComponent } from './entity-details-list/entity-details-list.component';
import { SharedModule } from '../shared/shared.module';
import { ElasticConfigService } from '../../../../fibi/src/app/common/services/elastic-config.service';
import { AddNewEntityDetailsComponent } from './add-new-entity-details/add-new-entity-details.component';
import { FormsModule } from '@angular/forms';
import { SharedComponentModule } from '../shared-components/shared-component.module';
import { EntityManagementService } from './entity-management.service';

const routes: Routes = [
  {
    path: '', component: EntityManagementComponent,
    children: [
      { path: '', redirectTo: 'entity-list', pathMatch: 'full' },
      { path: 'entity-details', component: EntityDetailsListComponent },
      { path: 'entity-list', component: EntityListComponent }]
  },
];

@NgModule({
  declarations: [
    EntityManagementComponent,
    EntityListComponent,
    ViewEntityDetailsComponent,
    EntityDetailsListComponent,
    AddNewEntityDetailsComponent
  ],
  imports: [
    CommonModule,
    RouterModule.forChild(routes),
    MatIconModule,
    SharedModule,
    FormsModule,
    SharedComponentModule,
  ],
  exports: [
  ],providers: [ElasticConfigService]
})
export class EntityManagementModule {
}
