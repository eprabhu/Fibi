import { NgModule } from '@angular/core';
import { CommonModule } from '@angular/common';
import { CostAllocationRetriggerComponent } from './cost-allocation-retrigger.component';
import { SharedModule } from '../../../../shared/shared.module';
import { FormsModule } from '@angular/forms';
import { RouterModule } from '@angular/router';

@NgModule({
  imports: [
    CommonModule,
    SharedModule,
    FormsModule,
    RouterModule.forChild([{ path: '', component: CostAllocationRetriggerComponent }])
  ],
  declarations: [CostAllocationRetriggerComponent]
})
export class CostAllocationRetriggerModule { }
