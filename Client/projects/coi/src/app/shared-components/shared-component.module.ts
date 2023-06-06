import { NgModule } from '@angular/core';
import { CommonModule } from '@angular/common';
import { NoInformationComponent } from './no-information/no-information.component';
import { MatIconModule } from '@angular/material/icon';
import { PersonalDetailsModalComponent } from './personal-details-modal/personal-details-modal.component';
import { PersonDetailsComponent } from './personal-details-modal/person-details/person-details.component';
import { TrainingDetailsComponent } from './personal-details-modal/Training-details/Training-details.component';
import { SharedModule } from '../shared/shared.module';
import { SharedSfiCardComponent } from './shared-sfi-card/shared-sfi-card.component';
import { DisclosureCreateModalComponent } from './disclosure-create-modal/disclosure-create-modal.component';
import { FormsModule } from '@angular/forms';
import { AssignAdministratorModalComponent } from './assign-administrator-modal/assign-administrator-modal.component';

@NgModule({
  imports: [
    CommonModule,
    MatIconModule,
    SharedModule,
    FormsModule
  ],
  declarations: [
    NoInformationComponent,
    PersonalDetailsModalComponent,
    PersonDetailsComponent,
    TrainingDetailsComponent,
    SharedSfiCardComponent,
    DisclosureCreateModalComponent,
    AssignAdministratorModalComponent,
  ],
  exports: [
    NoInformationComponent,
    PersonalDetailsModalComponent,
    SharedSfiCardComponent,
    DisclosureCreateModalComponent,
    AssignAdministratorModalComponent
  ]
})
export class SharedComponentModule { }
