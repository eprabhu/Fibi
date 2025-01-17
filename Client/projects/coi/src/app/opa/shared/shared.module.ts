import {NgModule} from '@angular/core';
import {CommonModule} from '@angular/common';
import {ReviewerActionModalComponent} from './reviewer-action-modal/reviewer-action-modal.component';
import { SharedModule } from '../../shared/shared.module';


@NgModule({
    declarations: [ReviewerActionModalComponent],
    imports: [
        CommonModule,
        SharedModule
    ],
    exports: [ReviewerActionModalComponent]
})
export class OPASharedModule {
}
