import {NgModule} from '@angular/core';
import {CommonModule} from '@angular/common';
import {CountModalComponent} from './count-modal/count-modal.component';
import {RouterModule} from '@angular/router';
import {FormsModule} from '@angular/forms';
import { CoiReviewCommentsComponent } from './coi-review-comments/coi-review-comments.component';
import { ActivityComponent } from '../activity-track/activity.component';

@NgModule({
    declarations: [
        CountModalComponent,
        CoiReviewCommentsComponent,
        ActivityComponent
    ],
    imports: [
        CommonModule,
        FormsModule,
        RouterModule
    ],
    exports: [
        CountModalComponent,
        CoiReviewCommentsComponent,
        ActivityComponent
    ]
})

export class CoiSharedModule {
}
