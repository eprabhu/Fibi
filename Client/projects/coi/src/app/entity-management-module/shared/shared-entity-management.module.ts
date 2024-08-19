import { NgModule } from '@angular/core';
import { CommonModule } from '@angular/common';
import { EntityCommonCardComponent } from './entity-common-card/entity-common-card.component';
import { SharedModule } from '../../shared/shared.module';
import { SharedComponentModule } from '../../shared-components/shared-component.module';
import { MatIconModule } from '@angular/material/icon';

@NgModule({
  declarations: [
    EntityCommonCardComponent
  ],
  imports: [
    CommonModule,
    SharedModule,
    SharedComponentModule,
    MatIconModule
  ],
  exports: [
    EntityCommonCardComponent
  ]
})
export class SharedEntityManagementModule { }
