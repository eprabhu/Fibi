import { NgModule } from '@angular/core';
import { CommonModule } from '@angular/common';
import { NoInformationComponent } from './no-information/no-information.component';
import { MatIconModule } from '@angular/material/icon';
import { personalDetailsModalComponent } from './personal-details-modal/personal-details-modal.component';
import { personDetailsComponent } from './personal-details-modal/person-details/person-details.component';
import { trainingDetailsComponent } from './personal-details-modal/Training-details/Training-details.component';
import { SharedModule } from '../shared/shared.module';
import { SharedSfiCardComponent } from './shared-sfi-card/shared-sfi-card.component';
import { DisclosureCreateModalComponent } from './disclosure-create-modal/disclosure-create-modal.component';
import { FormsModule } from '@angular/forms';

@NgModule({
  imports: [
    CommonModule,
    MatIconModule,
    SharedModule,
    FormsModule
  ],
  declarations: [NoInformationComponent,personalDetailsModalComponent,personDetailsComponent,trainingDetailsComponent, SharedSfiCardComponent, DisclosureCreateModalComponent],
  exports: [NoInformationComponent,personalDetailsModalComponent,SharedSfiCardComponent,DisclosureCreateModalComponent]
})
export class SharedComponentModule { }
