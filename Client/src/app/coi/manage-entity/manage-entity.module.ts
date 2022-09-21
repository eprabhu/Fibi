import { NgModule } from '@angular/core';
import { FormsModule } from '@angular/forms';
import { CommonModule } from '@angular/common';
import { RouterModule } from '@angular/router';

import { ManageEntityComponent } from './manage-entity.component';
import { EntityListComponent } from './entity-list/entity-list.component';
import { ViewEntityComponent } from './view-entity/view-entity.component';
import { SharedModule } from '../../shared/shared.module';
import { DataStoreService } from '../services/data-store.service';

const routes = [
  {
    path: '', component: ManageEntityComponent,
    children: [
      { path: '', redirectTo: 'entity-list', pathMatch: 'full' },
      {
        path: 'entity-list', component: EntityListComponent
      },
      {
        path: 'view-entity', component: ViewEntityComponent
      }
    ]
  }];

@NgModule({
  imports: [
    CommonModule,
    RouterModule.forChild(routes),
    FormsModule,
    SharedModule
  ],
  declarations: [
    ManageEntityComponent,
    EntityListComponent,
    ViewEntityComponent
  ],
  providers: [
    DataStoreService
  ]
})
export class ManageEntityModule { }
