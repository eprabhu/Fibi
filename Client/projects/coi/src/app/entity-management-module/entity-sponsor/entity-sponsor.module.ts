import { NgModule } from '@angular/core';
import { CommonModule } from '@angular/common';
import { RouterModule } from '@angular/router';
import { EntitySponsorComponent } from './entity-sponsor.component';

@NgModule({
  declarations: [EntitySponsorComponent],
  imports: [
    CommonModule,
    RouterModule.forChild([{ path: '', component: EntitySponsorComponent}]),
  ]
})
export class EntitySponsorModule { }
