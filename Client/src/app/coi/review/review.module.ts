import { NgModule } from '@angular/core';
import { CommonModule } from '@angular/common';
import { RouterModule } from '@angular/router';
import { FormsModule } from '@angular/forms';

import { ReviewComponent } from './review.component';
import { SharedModule } from '../../shared/shared.module';
import { ReviewService } from './review.service';
import { LocationComponent } from './location-track/location.component';
import { GeneralCommentsComponent } from './general-comments/general-comments.component';
import { CoiSharedModule } from '../shared/shared.module';

@NgModule({
    imports: [
        CommonModule,
        RouterModule.forChild([{ path: '', component: ReviewComponent }]),
        FormsModule,
        SharedModule,
        CoiSharedModule
    ],
    declarations: [
        ReviewComponent,
        LocationComponent,
        GeneralCommentsComponent
    ],
    providers: [ReviewService]
})
export class ReviewModule {
}
