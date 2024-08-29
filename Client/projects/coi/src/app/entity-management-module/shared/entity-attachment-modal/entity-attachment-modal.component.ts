import { Component, EventEmitter, HostListener, Input, OnInit, Output } from '@angular/core';
import { Subscription } from 'rxjs';
import { CommonService } from '../../../common/services/common.service';
import { hideModal, isEmptyObject, openModal } from 'projects/fibi/src/app/common/utilities/custom-utilities';
import { subscriptionHandler } from 'projects/fibi/src/app/common/utilities/subscription-handler';
import { Router } from '@angular/router';
import { EntireEntityDetails, EntityAttachmentDetails, NewAttachments, SaveAttachmentRo } from '../entity-interface';
import { EntityAttachmentModalService } from './entity-attachment-modal.service';
import { EntityDataStoreService } from '../../entity-data-store.service';

@Component({
    selector: 'app-entity-attachment-modal',
    templateUrl: './entity-attachment-modal.component.html',
    styleUrls: ['./entity-attachment-modal.component.scss']
})
export class EntityAttachmentModalComponent implements OnInit {

    @Input() attachmentHelpText: string;
    @Output() closeModal = new EventEmitter<boolean>(); //close event.
    attachmentDetails: any;
    uploadedFiles = [];
    entityAttachmentRO: EntityAttachmentDetails = new EntityAttachmentDetails()
    entityAttachmentSaveRO: SaveAttachmentRo = new SaveAttachmentRo();
    newAttachments: Array<NewAttachments> = [];
    selectedAttachmentDescriptions = [];
    attachmentTypes = [];
    selectedAttachmentTypeCodes: any = [];
    attachmentErrorMsg = '';
    $subscriptions: Subscription[] = [];
    isSaving = false;
    entityAttachmentTypeOption = 'ENTITY_ATTACHMENT_TYPE#ATTACHMENT_TYPE_CODE#false#false';
    entityAttachmentSectionCode = 'DESCRIPTION#ENTITY_SECTION_CODE#false#false';
    selectedLookUpList: any[] = [];
    selectedAttachmentType: any
    entityDetails: any;
    sectionCode: any;
    entityId: string;
    // helpTexts = `You can view and edit attachments under the 'My Attachments' tab.`;

    constructor(private _commonService: CommonService, public _router: Router,
        private _entityAttachmentModalService: EntityAttachmentModalService,
        private _dataStoreService: EntityDataStoreService
    ) { }

    ngOnInit() {
        // this.getAttachmentType();
        openModal('addAttachmentModal', {
            backdrop: 'static',
            keyboard: false,
            focus: true
        });
        this.getDataFromStore();
        this.listenDataChangeFromStore();
        this.setData();
    }

    setData() {
        this.sectionCode = this.entityDetails?.sectionCode;
        this.entityId = this.entityDetails.entityId;
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

    // saveAttachments(): void {
    //     this.checkMandatory();
    //     if (!this.attachmentErrorMsg && !this.isSaving) {
    //         this.isSaving = true;
    //         this.updateAddAttachmentDetails();
    //         this.$subscriptions.push(this._attachmentService.addAttachment({
    //             'personId': this._commonService.getCurrentUserDetail('personID'),
    //             'newAttachments': this.newAttachments
    //         }, this.uploadedFiles).subscribe((data: any) => {
    //             this.isSaving = false;
    //             this.clearAttachments();
    //             if (this._router.url.includes('/coi/user-dashboard/attachments')) {
    //                 this._commonService.$updateLatestAttachment.next(data);
    //             }
    //             this._commonService.showToast(HTTP_SUCCESS_STATUS, "Attachment(s) added successfully.");
    //         }, err => {
    //             this.isSaving = false;
    //             this._commonService.showToast(HTTP_ERROR_STATUS, "Error in adding attachment(s), please try again");
    //         }));
    //     }
    // }

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

    //   addAttachment(params, uploadedFile) {
    //     const formData = new FormData();
    //     for (const file of uploadedFile) {
    //         formData.append('files', file, file.name);
    //     }
    //     formData.append('formDataJson', JSON.stringify(params));
    //     return this._http.post(this._commonService.baseUrl + '/saveOrUpdateAttachments', formData);
    // }

    // getAttachmentTypes() {
    //     return this._http.get(this._commonService.baseUrl + '/loadDisclAttachTypes');
    // }

    onAttachmentTypeSelected() {
        if (event) {
            this.entityAttachmentRO.attachmentTypeCode = event[0].code;
            this.selectedAttachmentType = event[0];

        } else {
            this.entityAttachmentRO.attachmentTypeCode = null;
            this.selectedAttachmentType = null;
        }
    }

    getAttachmentDetails(sectionCode, entityId) {
        this.$subscriptions.push(this._entityAttachmentModalService.getAttachmentDetails(sectionCode, entityId).subscribe((data) => {
            this.attachmentDetails = data;
        }))
    }


    // private getAttachmentType(): void {
    //     this.$subscriptions.push(this._attachmentService.getAttachmentTypes().subscribe((data: any) => {
    //         if (data) {
    //             this.attachmentTypes = data;
    //         }
    //     }, err => {
    //         this._commonService.showToast(HTTP_ERROR_STATUS, 'Error in fetching attachment type, please try again');
    //     }));
    // }

    private listenDataChangeFromStore(): void {
        this.$subscriptions.push(
            this._dataStoreService.dataEvent.subscribe((dependencies: string[] | 'ENTITY_RISK_TYPE') => {
                if (dependencies !== 'ENTITY_RISK_TYPE') {
                    this.getDataFromStore();
                }
            })
        );
    }

    private getDataFromStore(): void {
        const ENTITY_DATA: EntireEntityDetails = this._dataStoreService.getData();
        if (isEmptyObject(ENTITY_DATA)) { return; }
        this.entityDetails = ENTITY_DATA.entityDetails;
    }

    saveAttachments() {
        this.attachmentDetails.sectionCode = '1';
        const ID = '';
        this.$subscriptions.push(this._entityAttachmentModalService.SaveAttachment(ID, this.newAttachments, this.uploadedFiles).subscribe((data) => {
        }))
    }

}
