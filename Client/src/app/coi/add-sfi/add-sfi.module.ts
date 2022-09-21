
import { NgModule } from '@angular/core';
import { CommonModule } from '@angular/common';
import { FormsModule } from '@angular/forms';
import { RouterModule } from '@angular/router';

import { AddSfiComponent } from './add-sfi.component';
import { SharedModule } from '../../shared/shared.module';
import { AddSfiService } from './add-sfi.service';
import { SfiQuestionnaireComponent } from './sfi-questionnaire/sfi-questionnaire.component';
import { AddSfiResolveService } from './add-sfi-resolve.service';
import { DataStoreService } from '../services/data-store.service';

@NgModule({
  imports: [
    CommonModule,
    SharedModule,
    FormsModule,
    RouterModule.forChild([{ path: '', component: AddSfiComponent, canActivate: [AddSfiResolveService] }])
  ],
  declarations: [
    AddSfiComponent,
    SfiQuestionnaireComponent
],
  providers: [AddSfiService, AddSfiResolveService, DataStoreService]
})

export class AddSfiModule { }
