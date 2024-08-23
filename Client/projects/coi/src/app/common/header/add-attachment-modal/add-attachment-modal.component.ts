import { Component, EventEmitter, HostListener, Input, OnInit, Output } from '@angular/core';
import { hideModal, openModal } from '../../../../../../fibi/src/app/common/utilities/custom-utilities';
import { AddAttachmentModalService } from './add-attachment-modal.service';
import { CommonService } from '../../services/common.service';
import { Subscription } from 'rxjs';
import { HTTP_ERROR_STATUS, HTTP_SUCCESS_STATUS } from '../../../app-constants';
import { subscriptionHandler } from '../../../../../../fibi/src/app/common/utilities/subscription-handler';
import { Router } from '@angular/router';

@Component({
    selector: 'app-add-attachment-modal',
    templateUrl: './add-attachment-modal.component.html',
    styleUrls: ['./add-attachment-modal.component.scss']
})
export class AddAttachmentModalComponent implements OnInit {

    uploadedFiles = [];
    newAttachments: Array<any> = [];
    selectedAttachmentDescriptions = [];
    attachmentTypes = [];
    selectedAttachmentTypeCodes: any = [];
    attachmentErrorMsg = '';
    $subscriptions: Subscription[] = [];
    isSaving = false;
    // helpTexts = `You can view and edit attachments under the 'My Attachments' tab.`;
    @Input() attachmentHelpText = '';
    @Output() closeModal = new EventEmitter<boolean>(); //close event.

    constructor(private _attachmentService: AddAttachmentModalService, private _commonService: CommonService, public _router: Router) { }

    ngOnInit() {
        this.getAttachmentType();
        openModal('addAttachmentModal', {
            backdrop: 'static',
            keyboard: false,
            focus: true
          });
    }

    private getAttachmentType(): void {
        this.$subscriptions.push(this._attachmentService.getAttachmentTypes().subscribe((data: any) => {
            if (data) {
                this.attachmentTypes = data;
            }
        }, err => {
            this._commonService.showToast(HTTP_ERROR_STATUS, 'Error in fetching attachment type, please try again');
        }));
    }

    fileDrop(files) {
        this.attachmentErrorMsg = '';
        for (let index = 0; index < files.length; index++) {
            this.uploadedFiles.push(files[index]);
        }
    }

    private updateAddAttachmentDetails(): void {
        this.uploadedFiles.forEach((ele, index) => {
            const attachment: any = {};
            attachment.fileName = ele.name;
            attachment.mimeType = ele.type;
            attachment.attaTypeCode = this.selectedAttachmentTypeCodes[index];
            attachment.description = this.selectedAttachmentDescriptions[index];
            attachment.fileDataId = null;
            this.newAttachments.push(attachment);
        });
    }

    deleteFromUploadedFileList(index: number): void {
        this.commonSplice(this.uploadedFiles, index);
        this.commonSplice(this.newAttachments, index);
        this.commonSplice(this.selectedAttachmentDescriptions, index);
        this.commonSplice(this.selectedAttachmentTypeCodes, index);
        this.attachmentErrorMsg = '';
    }

    private commonSplice(arrayName, index): void {
        arrayName.splice(index, 1);
    }

    saveAttachments(): void {
        this.checkMandatory();
        if (!this.attachmentErrorMsg && !this.isSaving) {
            this.isSaving = true;
            this.updateAddAttachmentDetails();
            this.$subscriptions.push(this._attachmentService.addAttachment({
                'personId': this._commonService.getCurrentUserDetail('personID'),
                'newAttachments': this.newAttachments
            }, this.uploadedFiles).subscribe((data: any) => {
                this.isSaving = false;
                this.clearAttachments();
                if (this._router.url.includes('/coi/user-dashboard/attachments')) {
                    this._commonService.$updateLatestAttachment.next(data);
                }
                this._commonService.showToast(HTTP_SUCCESS_STATUS, "Attachment(s) added successfully.");
            }, err => {
                this.isSaving = false;
                this._commonService.showToast(HTTP_ERROR_STATUS, "Error in adding attachment(s), please try again");
            }));
        }
    }

    private checkMandatory(): void {
        this.attachmentErrorMsg = '';
        if (!this.uploadedFiles.length) {
            this.attachmentErrorMsg = '* Please choose at least one document';
        }
        this.uploadedFiles.forEach((ele, index) => {
            if (this.selectedAttachmentTypeCodes[index] == null || !this.selectedAttachmentTypeCodes[index]) {
                this.attachmentErrorMsg = '* Please select attachment type for each attachment';
            }
        });
    }

    clearAttachments(): void {
        hideModal('addAttachmentModal');
        this.uploadedFiles = [];
        this.newAttachments = [];
        this.selectedAttachmentDescriptions = [];
        this.selectedAttachmentTypeCodes = [];
        this.closeModal.emit(true);
    }

    clearValidation(): void {
        this.attachmentErrorMsg = '';
    }

    ngOnDestroy(): void {
        subscriptionHandler(this.$subscriptions);
    }

    @HostListener('document:keydown.escape', ['$event'])
    handleEscapeEvent(event: any): void {
        if ((event.key === 'Escape' || event.key === 'Esc')) {
            this.clearAttachments();
        }
    }
}
