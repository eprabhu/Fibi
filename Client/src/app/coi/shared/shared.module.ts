import {NgModule} from '@angular/core';
import {CommonModule} from '@angular/common';
import {CountModalComponent} from './count-modal/count-modal.component';
import {SharedModule} from '../../shared/shared.module';
import {RouterModule} from '@angular/router';
import {FormsModule} from '@angular/forms';
import { CoiReviewCommentsComponent } from './coi-review-comments/coi-review-comments.component';

@NgModule({
    declarations: [
        CountModalComponent,
        CoiReviewCommentsComponent
    ],
    imports: [
        CommonModule,
        SharedModule,
        FormsModule,
        RouterModule
    ],
    exports: [
        CountModalComponent,
        CoiReviewCommentsComponent
    ]
})

export class CoiSharedModule {
}
