import { NgModule } from '@angular/core';
import { CommonModule } from '@angular/common';
import { TravelRiskSliderComponent } from './travel-risk-slider.component';
import { TravelRiskSliderService } from './travel-risk-slider.service';
import { CoiSummaryEventsAndStoreService } from '../../disclosure/summary/coi-summary-events-and-store.service';
import { SharedComponentModule } from '../../shared-components/shared-component.module';
import { FormsModule } from '@angular/forms';
import { SharedModule } from '../../shared/shared.module';

@NgModule({
    imports: [
        CommonModule,
        SharedComponentModule,
        FormsModule,
        SharedModule
    ],
    declarations: [TravelRiskSliderComponent],
    exports: [TravelRiskSliderComponent],
    providers: [TravelRiskSliderService, CoiSummaryEventsAndStoreService]
})

export class TravelRiskSliderModule { }


