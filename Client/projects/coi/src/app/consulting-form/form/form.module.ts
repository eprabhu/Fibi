import { NgModule } from '@angular/core';
import { CommonModule } from '@angular/common';
import { RouterModule } from '@angular/router';
import { FormComponent } from './form.component';
import { SharedModule } from '../../shared/shared.module';
import { FormService } from './form-service.service';
import { AddSfiModule } from '../../add-sfi/add-sfi.module';
import { FormsModule } from '@angular/forms';

@NgModule({
  declarations: [
    FormComponent
  ],
  imports: [
    CommonModule,
    RouterModule.forChild([{ path: '', component: FormComponent }]),
    SharedModule,
    AddSfiModule,
    FormsModule
  ],
  providers: [
    FormService
  ]
})
export class FormModule { }
