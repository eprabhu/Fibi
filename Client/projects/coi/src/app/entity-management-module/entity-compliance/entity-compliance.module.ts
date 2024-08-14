import { NgModule } from '@angular/core';
import { CommonModule } from '@angular/common';
import { EntityComplianceComponent } from './entity-compliance.component';
import { RouterModule } from '@angular/router';

@NgModule({
  declarations: [
    EntityComplianceComponent
  ],
  imports: [
    CommonModule,
    RouterModule.forChild([{ path: '', component: EntityComplianceComponent}]),
  ]
})
export class EntityComplianceModule { }
