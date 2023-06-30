import { NgModule } from '@angular/core';
import { CommonModule } from '@angular/common';
import { ReviewerActionModalComponent } from './reviewer-action-modal/reviewer-action-modal.component';

@NgModule({
  imports: [
    CommonModule
  ],
  declarations: [ReviewerActionModalComponent],
  exports: [ReviewerActionModalComponent],
  providers: []
})
export class SharedDisclosureModule { }
