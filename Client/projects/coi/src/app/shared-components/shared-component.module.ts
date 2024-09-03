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
 import { EntityRiskSliderComponent } from './entity-risk-slider/entity-risk-slider.component';
import { ReviewCommentsSliderComponent } from './review-comments-slider/review-comments-slider.component';
import { CoiSliderComponent } from './coi-slider/coi-slider.component';
import { HelpTextComponent } from './help-text/help-text.component';
import { PersonProjectEntityCardComponent } from './person-project-entity-card/person-project-entity-card.component';
import { ReviewCommentListViewComponent } from './review-comment-list-view/review-comment-list-view.component';
import { CoiReviewCommentsComponent } from './coi-review-comments/coi-review-comments.component';
import { ConcurrencyWarningModalComponent } from './concurrency-warning-modal/concurrency-warning-modal.component';
import { EntityDetailsCardComponent } from './entity-details-card/entity-details-card.component';
import { SharedProjectDetailsModalComponent } from './shared-project-details-modal/shared-project-details-modal.component';
import { CoiModalComponent } from './coi-modal/coi-modal.component';
import { ScrollSpyComponent } from './scroll-spy/scroll-spy.component';
import { CoiCountModalComponent } from './coi-count-modal/coi-count-modal.component';

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
    HelpTextComponent,
    PersonProjectEntityCardComponent,
    EntityRiskSliderComponent,
    ReviewCommentListViewComponent,
    CoiReviewCommentsComponent,
    ConcurrencyWarningModalComponent,
    SharedProjectDetailsModalComponent,
    EntityDetailsCardComponent,
    CoiModalComponent,
    ScrollSpyComponent,
    CoiCountModalComponent
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
    HelpTextComponent,
    PersonProjectEntityCardComponent,
    EntityRiskSliderComponent,
    ReviewCommentListViewComponent,
    CoiReviewCommentsComponent,
    ConcurrencyWarningModalComponent,
    EntityDetailsCardComponent,
    SharedProjectDetailsModalComponent,
    CoiModalComponent,
    ScrollSpyComponent,
    CoiCountModalComponent
  ]
})
export class SharedComponentModule { }
