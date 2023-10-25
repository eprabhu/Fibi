import {NgModule} from '@angular/core';
import {CommonModule} from '@angular/common';
import {ReviewerActionModalComponent} from './reviewer-action-modal/reviewer-action-modal.component';


@NgModule({
    declarations: [ReviewerActionModalComponent],
    imports: [
        CommonModule
    ],
    exports: [ReviewerActionModalComponent]
})
export class OPASharedModule {
}
