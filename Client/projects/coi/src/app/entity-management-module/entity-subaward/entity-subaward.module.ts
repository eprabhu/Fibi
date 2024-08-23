import { NgModule } from '@angular/core';
import { CommonModule } from '@angular/common';
import { EntitySubawardComponent } from './entity-subaward.component';
import { RouterModule } from '@angular/router';



@NgModule({
  declarations: [
    EntitySubawardComponent
  ],
  imports: [
    CommonModule,
    RouterModule.forChild([{ path: '', component: EntitySubawardComponent}]),
  ]
})
export class EntitySubawardModule { }
