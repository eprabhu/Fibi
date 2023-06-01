import { NgModule } from '@angular/core';
import { CommonModule } from '@angular/common';
import { RouterModule } from '@angular/router';
import { TravelQuestionnaireComponent } from './travel-questionnaire.component';
import { SharedModule } from '../../shared/shared.module';

@NgModule({
    imports: [
        CommonModule,
        SharedModule,
        RouterModule.forChild([{ path: '', component: TravelQuestionnaireComponent }])
    ],
    declarations: [TravelQuestionnaireComponent]
})
export class TravelQuestionnaireModule { }
