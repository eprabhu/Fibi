import { NgModule } from '@angular/core';
import { CommonModule } from '@angular/common';
import { TravelSummaryComponent } from './travel-summary.component';
import { TravelReviewComponent } from './travel-review/travel-review.component';
import { FormsModule } from '@angular/forms';
import { RouterModule, Routes } from '@angular/router';
import { TravelToolKitComponent } from './travel-tool-kit/travel-tool-kit.component';
import { MatIconModule } from '@angular/material/icon';
import { SharedComponentModule } from '../../shared-components/shared-component.module';
import { SharedModule } from '../../shared/shared.module';
import { TravelFormSummaryComponent } from './travel-review/travel-form-summary/travel-form-summary.component';
import { TravelCertifySummaryComponent } from './travel-review/travel-certify-summary/travel-certify-summary.component';

const routes: Routes = [
    {
        path: '', component: TravelSummaryComponent,
    }
];

@NgModule({
    declarations: [
        TravelSummaryComponent,
        TravelReviewComponent,
        TravelToolKitComponent,
        TravelFormSummaryComponent,
        TravelCertifySummaryComponent
    ],
    imports: [
        CommonModule,
        SharedModule,
        FormsModule,
        RouterModule.forChild(routes),
        SharedComponentModule,
        MatIconModule
    ]
})
export class TravelSummaryModule { }
