import { NgModule } from '@angular/core';
import { CommonModule } from '@angular/common';
import { EntityNotesComponent } from './entity-notes.component';
import { RouterModule } from '@angular/router';
import { EntitySponsorComponent } from '../entity-sponsor/entity-sponsor.component';
import { SharedComponentModule } from '../../shared-components/shared-component.module';

@NgModule({
  declarations: [
    EntityNotesComponent
  ],
  imports: [
    CommonModule,
    SharedComponentModule,
    RouterModule.forChild([{ path: '', component: EntityNotesComponent}]),
  ]
})
export class EntityNotesModule { }
