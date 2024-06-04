import { NgModule } from '@angular/core';
import { CommonModule } from '@angular/common';
import { EntityManagementComponent } from './entity-management.component';
import { RouterModule, Routes } from '@angular/router';
import { MatIconModule } from '@angular/material/icon';
import { EntityDetailsListComponent } from './entity-details-list/entity-details-list.component';
import { SharedModule } from '../shared/shared.module';
import { FormsModule } from '@angular/forms';
import { SharedComponentModule } from '../shared-components/shared-component.module';
import { EntityManagementGuardService } from './entity-management-guard.service';
import { SfiService } from '../disclosure/sfi/sfi.service';
import { EntityDetailsService } from '../disclosure/entity-details/entity-details.service';
import { EntityHistoryComponent } from './entity-history/entity-history.component';
import { EntityDetailsModule } from '../disclosure/entity-details/entity-details.module';
import { ViewEntityDetailsComponent } from './view-entity-details/view-entity-details.component';
import { EntityListComponent } from './entity-list/entity-list.component';
import { EntityManagementService } from './entity-management.service';
import { SharedLibraryModule } from '../../../../shared/src/lib/shared.module' ;

const routes: Routes = [
  {
    path: '', component: EntityManagementComponent , canActivate: [EntityManagementGuardService],
    children: [
      { path: '', redirectTo: 'entity-list', pathMatch: 'full' },
      { path: 'entity-details', component: EntityDetailsListComponent },
      { path: 'entity-list', component: EntityListComponent }
    ]
  },
];

@NgModule({
  declarations: [
    EntityManagementComponent,
    EntityDetailsListComponent,
    EntityHistoryComponent,
    EntityListComponent,
    ViewEntityDetailsComponent
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
  ],
  exports: [
  ],
  providers: [EntityDetailsService, EntityManagementGuardService, SfiService,
     EntityManagementGuardService, EntityManagementService]
})
export class EntityManagementModule {
}
