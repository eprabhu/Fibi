import { NgModule } from '@angular/core';
import { CommonModule } from '@angular/common';
import { PositionRetriggerComponent } from './position-retrigger.component';
import { RouterModule } from '@angular/router';
import { FormsModule } from '@angular/forms';
import { SharedModule } from '../../../../shared/shared.module';

@NgModule({
  imports: [
    CommonModule,
    FormsModule,
    SharedModule,
    RouterModule.forChild([{path: '', component: PositionRetriggerComponent}])
  ],
  declarations: [PositionRetriggerComponent]
})
export class PositionRetriggerModule { }
