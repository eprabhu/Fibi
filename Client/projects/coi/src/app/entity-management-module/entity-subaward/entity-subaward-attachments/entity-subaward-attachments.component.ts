import { Component, Input, OnInit } from '@angular/core';

@Component({
    selector: 'app-entity-subaward-attachments',
    templateUrl: './entity-subaward-attachments.component.html',
    styleUrls: ['./entity-subaward-attachments.component.scss']
})
export class EntitySubawardAttachmentsComponent implements OnInit {

    @Input() sectionName: any;
    @Input() sectionId: any;

    entityAttachmentHelpText: string = '';
    isOpenSubawardAttachmentModal = false;


    constructor() { }

    ngOnInit() {
    }

    closeModal(event: any): void {
        this.isOpenSubawardAttachmentModal = false;
        this.entityAttachmentHelpText = '';
    }

    openAttachmentModal() {
        this.isOpenSubawardAttachmentModal = true
        this.entityAttachmentHelpText = 'You can view and edit attachments under the subaward tab.';
    }

}
