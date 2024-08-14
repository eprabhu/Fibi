import { NgModule } from '@angular/core';
import { CommonModule } from '@angular/common';
import { RouterModule } from '@angular/router';
import { EntityHistoryComponent } from './entity-history.component';

@NgModule({
  declarations: [
    EntityHistoryComponent
  ],
  imports: [
    CommonModule,
    RouterModule.forChild([{ path: '', component: EntityHistoryComponent}]),
  ]
})
export class EntityHistoryModule { }
