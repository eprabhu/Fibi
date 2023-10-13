import { NgModule } from '@angular/core';
import { CommonModule } from '@angular/common';
import { SharedModule } from '../../../shared/shared.module';
import { AddAttachmentModalComponent } from './add-attachment-modal.component';
import { AddAttachmentModalService } from './add-attachment-modal.service';
import { CommonService } from '../../services/common.service';
import { FormsModule } from '@angular/forms';

@NgModule({
    imports: [
        CommonModule,
        SharedModule,
        FormsModule
    ],
    declarations: [AddAttachmentModalComponent],
    exports: [AddAttachmentModalComponent],
    providers: [AddAttachmentModalService, CommonService]
})
export class AddAttachmentModalModule { }
