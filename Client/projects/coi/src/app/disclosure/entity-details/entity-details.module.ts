import { NgModule } from '@angular/core';
import { CommonModule } from '@angular/common';
import { EntityDetailsComponent } from './entity-details.component';
import { RouterModule, Routes } from '@angular/router';
import { MatIconModule } from '@angular/material/icon';
import { AddRelationshipComponent } from './add-relationship/add-relationship.component';
import { AddEntityDetailsComponent } from './add-entity-details/add-entity-details.component';
import { SharedModule } from '../../shared/shared.module';
import { EntityQuestionnaireComponent } from './entity-questionnaire/entity-questionnaire.component';
import { EntityDetailsService } from './entity-details.service';
import { EntityDetailsGuardService } from './entity-details-guard.service';
import { FormsModule } from '@angular/forms';
import { SharedComponentModule } from '../../shared-components/shared-component.module';
import { SfiModule } from '../sfi/sfi.module';
import { DataStoreService } from '../services/data-store.service';
import { CoiService } from '../services/coi.service';
import { ViewSfiDetailsComponent } from './add-entity-details/view-sfi-details/view-sfi-details.component';
import { ViewRelationshipDetailsComponent } from './add-entity-details/view-relationship-details/view-relationship-details.component';

const routes: Routes = [{ path: '', component:EntityDetailsComponent, canActivate:[EntityDetailsGuardService] }]

@NgModule({
  imports: [
    CommonModule,
    RouterModule.forChild(routes),
    MatIconModule,
    SharedModule,
    FormsModule,
    SharedComponentModule,
    // SfiModule
  ],
  declarations: [
    EntityDetailsComponent,
    AddEntityDetailsComponent,
    AddRelationshipComponent,
    EntityQuestionnaireComponent,
    ViewSfiDetailsComponent,
    ViewRelationshipDetailsComponent,
  ],
  providers:[
    EntityDetailsService,
    EntityDetailsGuardService
  ]
})
export class EntityDetailsModule { }
