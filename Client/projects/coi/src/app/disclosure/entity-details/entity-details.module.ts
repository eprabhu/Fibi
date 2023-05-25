import { NgModule } from '@angular/core';
import { CommonModule } from '@angular/common';
import { EntityDetailsComponent } from './entity-details.component';
import { RouterModule, Routes } from '@angular/router';
import { MatIconModule } from '@angular/material/icon';
import { SharedModule } from '../../shared/shared.module';
import { EntityQuestionnaireComponent } from './entity-questionnaire/entity-questionnaire.component';
import { EntityDetailsService } from './entity-details.service';
import { FormsModule } from '@angular/forms';
import { SharedComponentModule } from '../../shared-components/shared-component.module';
import { ViewRelationshipDetailsComponent } from './view-relationship-details/view-relationship-details.component';
import { SfiService } from '../sfi/sfi.service';

const routes: Routes = [{ path: '', component:EntityDetailsComponent }]

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
    EntityQuestionnaireComponent,
    ViewRelationshipDetailsComponent,
  ],
  providers:[
    EntityDetailsService,
    ]
})
export class EntityDetailsModule { }
