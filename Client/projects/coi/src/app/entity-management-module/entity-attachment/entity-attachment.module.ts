import { NgModule } from '@angular/core';
import { CommonModule } from '@angular/common';
import { RouterModule } from '@angular/router';
import { EntityAttachmentComponent } from './entity-attachment.component';


@NgModule({
  declarations: [
    EntityAttachmentComponent
  ],
  imports: [
    CommonModule,
    RouterModule.forChild([{ path: '', component: EntityAttachmentComponent}]),
  ]
})
export class EntityAttachmentModule { }
