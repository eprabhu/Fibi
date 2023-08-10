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
import { AddSfiModule } from '../add-sfi/add-sfi.module';
import { AddSfiSliderComponent } from './add-sfi-slider/add-sfi-slider.component';
import { SliderCloseBtnComponent } from './slider-close-btn/slider-close-btn.component';
import { ActivateInactivateSfiModalComponent } from './activate-inactivate-sfi-modal/activate-inactivate-sfi-modal.component';
import { ConfirmationModalComponent } from './confirmation-modal/confirmation-modal.component';
import { ReviewCommentsSliderComponent } from './review-comments-slider/review-comments-slider.component';
import { CoiSliderComponent } from './coi-slider/coi-slider.component';
import { HelpTextComponent } from './help-text/help-text.component';
@NgModule({
  imports: [
    CommonModule,
    MatIconModule,
    SharedModule,
    FormsModule,
    AddSfiModule
  ],
  declarations: [
    NoInformationComponent,
    PersonalDetailsModalComponent,
    PersonDetailsComponent,
    TrainingDetailsComponent,
    SharedSfiCardComponent,
    DisclosureCreateModalComponent,
    AssignAdministratorModalComponent,
    AddSfiSliderComponent,
    SliderCloseBtnComponent,
    ActivateInactivateSfiModalComponent,
    ConfirmationModalComponent,
    ReviewCommentsSliderComponent,
    CoiSliderComponent,
    HelpTextComponent
  ],
  exports: [
    NoInformationComponent,
    PersonalDetailsModalComponent,
    SharedSfiCardComponent,
    DisclosureCreateModalComponent,
    AssignAdministratorModalComponent,
    AddSfiSliderComponent,
    SliderCloseBtnComponent,
    ActivateInactivateSfiModalComponent,
    ConfirmationModalComponent,
    ReviewCommentsSliderComponent,
    CoiSliderComponent,
    HelpTextComponent
  ]
})
export class SharedComponentModule { }
