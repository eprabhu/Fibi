import { NgModule } from '@angular/core';
import { CommonModule } from '@angular/common';
import { AddSfiComponent } from './add-sfi.component';
import { FormsModule } from '@angular/forms';
import { RouterModule, Routes } from '@angular/router';
import { SharedModule } from '../shared/shared.module';

const routes: Routes = [
  {
    path: 'create', component: AddSfiComponent
  }];

@NgModule({
  imports: [
    CommonModule,
    FormsModule,
    SharedModule,
    RouterModule.forChild(routes)
  ],
  declarations: [AddSfiComponent],
  exports: [AddSfiComponent]
})
export class AddSfiModule { }
