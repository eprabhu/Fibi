import { NgModule } from '@angular/core';
import { CommonModule } from '@angular/common';
import { RouterModule } from '@angular/router';
import { FormsModule } from '@angular/forms';

import { ReviewComponent } from './review.component';
import { ReviewService } from './review.service';
import { LocationComponent } from './location-track/location.component';
import { GeneralCommentsComponent } from './general-comments/general-comments.component';
import {SharedModule} from "../../shared/shared.module";
import { SharedDisclosureModule } from '../shared-disclosure/shared-disclosure.module';
import { ReviewHistoryComponent } from './review-history/review-history.component';
import { SharedComponentModule } from '../../shared-components/shared-component.module';

@NgModule({
    imports: [
        CommonModule,
        RouterModule.forChild([{ path: '', component: ReviewComponent }]),
        FormsModule,
        SharedModule,
        SharedDisclosureModule,
        SharedComponentModule
      ],
    declarations: [
        ReviewComponent,
        LocationComponent,
        GeneralCommentsComponent,
        ReviewHistoryComponent
    ],
    providers: [ReviewService]
})
export class ReviewModule {
}
