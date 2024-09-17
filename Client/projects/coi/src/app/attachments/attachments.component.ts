import { Component, Input, OnInit } from '@angular/core';
import { Subscription } from 'rxjs';
import { AttachmentsService } from './attachments.service';
import { CommonService } from '../common/services/common.service';
import { subscriptionHandler } from '../../../../fibi/src/app/common/utilities/subscription-handler';
import { HTTP_ERROR_STATUS, HTTP_SUCCESS_STATUS } from '../app-constants';
import { AttachmentInputType, COIAttachment, UpdateAttachmentEvent } from './attachment-interface';
import { deepCloneObject, fileDownloader } from 'projects/fibi/src/app/common/utilities/custom-utilities';
import { COIModalConfig, ModalActionEvent } from '../shared-components/coi-modal/coi-modal.interface';
import { closeCommonModal, openCommonModal } from '../common/utilities/custom-utilities';

@Component({
    selector: 'app-attachments',
    templateUrl: './attachments.component.html',
    styleUrls: ['./attachments.component.scss']
})
export class AttachmentsComponent implements OnInit {

    @Input() personId: any = null;
    @Input() isViewMode: boolean = false;

    attachmentLists: COIAttachment[] = [];
    isLoading = false;
    isSaving = false;
    $subscriptions: Subscription[] = [];
    attachmentInputType: AttachmentInputType = '';
    currentAttachment: COIAttachment;
    filteredCoiAttachmentsList: COIAttachment[] = [];
    updateIndex: number = null;
    isOpenConfirmationModal = false;
    isOpenVersionModal = false;
    CONFIRMATION_MODAL_ID: string = 'coi-attachment-delete-confirm-modal';
    VERSION_MODAL_ID: string = 'coi-attachment-version-modal';
    versionModalConfig = new COIModalConfig(this.VERSION_MODAL_ID, '', 'Close', 'xl');
    confirmationModalConfig = new COIModalConfig(this.CONFIRMATION_MODAL_ID, 'Delete Attachment', 'Cancel');


    constructor(private _attachmentService: AttachmentsService, public commonService: CommonService) { }

    ngOnInit(): void {
        this.fetchAllAttachments();
        this.applyAttachmentChanges();
    }

    ngOnDestroy(): void {
        subscriptionHandler(this.$subscriptions);
    }

    private applyAttachmentChanges(): void {
        this.$subscriptions.push(
            this.commonService.$updateLatestAttachment
                .subscribe((event: UpdateAttachmentEvent) => {
                    this.setUpdatedAttachments(event);
                }));
    }

    /**  */
    private setUpdatedAttachments(event: {
        attachment: COIAttachment[] | COIAttachment | null,
        attachmentInputType: 'REPLACE' | 'ADD' | 'DESCRIPTION_CHANGE'
    }) {
        const attachment = event.attachment;
        if (attachment === null) return;
        if (Array.isArray(attachment)) {
            // Add new attachments to the beginning of the list
            this.attachmentLists = [...attachment, ...this.attachmentLists];
        } else {
            const index = this.attachmentLists.findIndex(att => att.attachmentId === attachment.attachmentId);
            if (index > -1) {
                // update existing attachment
                this.attachmentLists[index] = deepCloneObject(attachment);
            }
        }
        this.filterLatestVersions();
    }

    private getPersonId() {
        return this.personId ? this.personId : this.commonService.getCurrentUserDetail('personID');
    }

    private fetchAllAttachments(): void {
        this.isLoading = true;
        this.$subscriptions.push(this._attachmentService.fetchAllAttachmentsForPerson(this.getPersonId()).subscribe((data: any) => {
            if (data) {
                this.attachmentLists = data;
                this.filterLatestVersions();
            }
            this.isLoading = false;
        }, (_err) => {
            this.isLoading = false;
            this.commonService.showToast(HTTP_ERROR_STATUS, "Error in fetching attachment list");
        }));
    }

    /**
     * Filters and retains only the latest versions of attachments.
     *
     * This method processes `attachmentLists` by grouping attachments based on their 
     * `attachmentNumber`. For attachments with multiple versions, it sorts them in 
     * descending order by `versionNumber` and keeps only the latest version. Older 
     * versions are stored in the `versionList` property of the latest attachment. 
     * If an attachment has no older versions, it is directly added to the final list.
     *
     * The filtered attachments are then stored in the `filteredCoiAttachmentsList`.
     */
    private filterLatestVersions(): void {
        const ATTACHMENTS_MAP = new Map<number, COIAttachment[]>();
        // Group attachments by attachmentNumber
        this.attachmentLists.forEach(attachment => {
            const { attachmentNumber } = attachment;
            const attachments = ATTACHMENTS_MAP.get(attachmentNumber) || [];
            attachments.push(attachment);
            ATTACHMENTS_MAP.set(attachmentNumber, attachments);
        });
        // Process the latest versions of each attachment
        this.filteredCoiAttachmentsList = Array.from(ATTACHMENTS_MAP.values()).map(attachments => {
            if (attachments.length > 1) {
                attachments.sort((a: COIAttachment, b: COIAttachment) => b.versionNumber - a.versionNumber);
                const [LATEST_ATTACHMENT, ...OLDER_VERSIONS] = attachments;
                LATEST_ATTACHMENT.versionList = OLDER_VERSIONS.length > 0 ? OLDER_VERSIONS : undefined;
                return LATEST_ATTACHMENT;
            }
            return attachments[0];
        });
    }

    openAttachmentModal(attachmentInputType: AttachmentInputType, currentAttachment: COIAttachment = null): void {
        this.currentAttachment = deepCloneObject(currentAttachment);
        this.attachmentInputType = attachmentInputType;
        this.commonService.openCommonAttachmentModal(attachmentInputType,  this.currentAttachment);
    }

    deleteConfirmModalActions(modalAction: ModalActionEvent): void {
        switch (modalAction.action) {
            case 'CLOSE_BTN':
            case 'SECONDARY_BTN':
                return this.closeConfirmationModal();
            case 'PRIMARY_BTN':
                return this.deleteAttachment();
            default: break;
        }
    }

    openDeleteConfirmModal(attachment: COIAttachment, deleteIndex: number): void {
        this.updateIndex = deleteIndex;
        this.isOpenConfirmationModal = true;
        this.currentAttachment = attachment;
        setTimeout(() => {
            openCommonModal(this.CONFIRMATION_MODAL_ID);
        }, 200);
    }

    private closeConfirmationModal(): void {
        closeCommonModal(this.CONFIRMATION_MODAL_ID);
        setTimeout(() => {
            this.currentAttachment = null;
            this.isOpenConfirmationModal = false;
            this.updateIndex = null;
        }, 200);
    }

    fileVersionModalActions(modalAction: ModalActionEvent): void {
        switch (modalAction.action) {
            case 'CLOSE_BTN':
            case 'SECONDARY_BTN':
                return this.closeVersionModal();
            default: break;
        }
    }

    openVersionModal(attachment: COIAttachment, versionIndex: number): void {
        this.updateIndex = versionIndex;
        this.isOpenVersionModal = true;
        this.currentAttachment = attachment;
        setTimeout(() => {
            openCommonModal(this.VERSION_MODAL_ID);
        }, 200);
    }

    private closeVersionModal(): void {
        closeCommonModal(this.VERSION_MODAL_ID);
        setTimeout(() => {
            this.currentAttachment = null;
            this.isOpenVersionModal = false;
            this.updateIndex = null;
        }, 200);
    }

    replaceAttachmentModal(currentAttachment: COIAttachment, updateIndex: number): void {
        this.currentAttachment = deepCloneObject(currentAttachment);
        this.openAttachmentModal('REPLACE', currentAttachment);
    }

    private deleteAttachment(): void {
        if(!this.isSaving){
            this.isSaving = true
            this.$subscriptions.push(
                this._attachmentService.deleteAttachment(this.currentAttachment?.attachmentNumber)
                    .subscribe((data: any) => {
                        this.deleteAttachmentAndVersions(this.updateIndex);
                        this.commonService.showToast(HTTP_SUCCESS_STATUS, 'Attachment deleted successfully.');
                        this.isSaving = false;
                    }, (_err) => {
                        this.commonService.showToast(HTTP_ERROR_STATUS, 'Attachment deleting failed.');
                        this.isSaving = false;
                    }));
        }
    }

    /**
     * Deletes an attachment and its versions from both `filteredCoiAttachmentsList` and `coiAttachmentsList`.
     *
     * @param index The index of the attachment to be deleted in the `filteredCoiAttachmentsList`.
     */
    private deleteAttachmentAndVersions(index: number): void {
        // Get the attachment to delete from filteredCoiAttachmentsList
        const ATTACHMENT_TO_DELETE = this.filteredCoiAttachmentsList[index];
        if (ATTACHMENT_TO_DELETE) {
            const { attachmentNumber } = ATTACHMENT_TO_DELETE;
            // Remove the attachment from filteredCoiAttachmentsList
            this.filteredCoiAttachmentsList.splice(index, 1);
            // Filter out all attachments with the same attachmentNumber from coiAttachmentsList
            this.attachmentLists = this.attachmentLists.filter(
                attachment => attachment.attachmentNumber !== attachmentNumber
            );
        }
        this.closeConfirmationModal();
    }

    downloadAttachment(attachment: COIAttachment): void {
        if (!this.isSaving) {
            this.isSaving = true;
            this.$subscriptions.push(
                this._attachmentService.downloadAwardAttachment(attachment?.attachmentId)
                    .subscribe((data: any) => {
                        fileDownloader(data, attachment.fileName);
                        this.commonService.showToast(HTTP_SUCCESS_STATUS, 'Attachment downloaded successfully');
                        this.isSaving = false;
                    }, (_err) => {
                        this.commonService.showToast(HTTP_ERROR_STATUS, 'Attachment downloading failed.');
                        this.isSaving = false;
            }));
        }
    }

    editCoiAttachment(currentAttachment: COIAttachment, editIndex: number): void {
        this.updateIndex = editIndex;
        this.openAttachmentModal('DESCRIPTION_CHANGE', currentAttachment);
    }
}
