import { NgModule } from '@angular/core';
import { CommonModule } from '@angular/common';

import { AttachmentComponent } from './attachment.component';
import { RouterModule } from '@angular/router';
import { SharedModule } from '../../shared/shared.module';
import { FormsModule } from '@angular/forms';

@NgModule({
    imports: [
        CommonModule,
        SharedModule,
        FormsModule,
        RouterModule.forChild([{ path: '', component: AttachmentComponent }]),
    ],
    declarations: [AttachmentComponent]
})
export class AttachmentModule { }
