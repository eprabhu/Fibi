import { NgModule } from '@angular/core';
import { CommonModule } from '@angular/common';
import { EntityNotesComponent } from './entity-notes.component';
import { RouterModule } from '@angular/router';
import { EntitySponsorComponent } from '../entity-sponsor/entity-sponsor.component';

@NgModule({
  declarations: [
    EntityNotesComponent
  ],
  imports: [
    CommonModule,
    RouterModule.forChild([{ path: '', component: EntityNotesComponent}]),
  ]
})
export class EntityNotesModule { }
