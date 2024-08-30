import { NgModule } from '@angular/core';
import { CommonModule } from '@angular/common';
import { RouterModule } from '@angular/router';
import { EntityAttachmentComponent } from './entity-attachment.component';
import { SharedComponentModule } from '../../shared-components/shared-component.module';


@NgModule({
  declarations: [
    EntityAttachmentComponent
  ],
  imports: [
    CommonModule,
    SharedComponentModule,
    RouterModule.forChild([{ path: '', component: EntityAttachmentComponent}]),
  ]
})
export class EntityAttachmentModule { }
