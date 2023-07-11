import { NgModule } from '@angular/core';
import { CommonModule } from '@angular/common';
import { UnsavedChangesWarningComponent } from './unsaved-changes-warning/unsaved-changes-warning.component';
import { TravelEntityCardComponent } from './travel-entity-card/travel-entity-card.component';
import { FormsModule } from '@angular/forms';
import { SharedComponentModule } from '../../shared-components/shared-component.module';
import { SharedModule } from '../../shared/shared.module';

@NgModule({
  imports: [
    CommonModule,
    SharedModule,
    FormsModule,
    SharedComponentModule,
  ],
  declarations: [
    UnsavedChangesWarningComponent,
    TravelEntityCardComponent
  ],
  exports: [
    UnsavedChangesWarningComponent,
    TravelEntityCardComponent
  ]
})
export class TravelSharedComponentModule { }
