import { NgModule } from '@angular/core';
import {AppAutocompleterComponent} from './app-autocompleter/app-autocompleter.component';
import {FormsModule} from '@angular/forms';
import {CommonModule} from '@angular/common';
import { GraphComponent } from './graph/graph.component';
import { TakeATourComponent } from './take-a-tour/take-a-tour.component';
import { DynamicPopoverComponent } from './dynamic-popover/dynamic-popover.component';
import { DragDirective } from './directives/drag.directive';
import { PersonCardComponent } from './graph/data-cards/person-card.component';
import { AwardCardComponent } from './graph/data-cards/award-card.component';
import { SponsorCardComponent } from './graph/data-cards/sponsor-card.component';
import { TravelDisclosureCardComponent } from './graph/data-cards/travel-card.component';
import { UnitCardComponent } from './graph/data-cards/unit-card.componenet';
import { ProposalCardComponent } from './graph/data-cards/proposal-card.component';
import { COICardComponent } from './graph/data-cards/coi-card.component';
import { EntityCardComponent } from './graph/data-cards/entity-card.componenet';
import { CountryCardComponent } from './graph/data-cards/country-card.component';

@NgModule({
  declarations: [
    AppAutocompleterComponent,
    GraphComponent,
    TakeATourComponent,
    DynamicPopoverComponent,
    DragDirective
  ],
  imports: [
      FormsModule,
      CommonModule,
      PersonCardComponent,
      AwardCardComponent,
      SponsorCardComponent,
      TravelDisclosureCardComponent,
      UnitCardComponent,
      ProposalCardComponent,
      COICardComponent,
      UnitCardComponent,
      EntityCardComponent,
      CountryCardComponent
  ],
  exports: [
    AppAutocompleterComponent,
    GraphComponent,
    TakeATourComponent,
    DynamicPopoverComponent,
    DragDirective
  ]
})
export class SharedLibraryModule { }
