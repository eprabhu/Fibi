import { NgModule } from '@angular/core';
import { CommonModule } from '@angular/common';
import { AssociationsComponent } from './associations.component';
import { RouterModule } from '@angular/router';
import { FormsModule } from '@angular/forms';
import { SharedModule } from '../../../shared/shared.module';

@NgModule({
  imports: [
    CommonModule,
    FormsModule,
    SharedModule,
    RouterModule.forChild([{path: '', component: AssociationsComponent}]),
  ],
  declarations: [AssociationsComponent]
})
export class AssociationsModule { }
