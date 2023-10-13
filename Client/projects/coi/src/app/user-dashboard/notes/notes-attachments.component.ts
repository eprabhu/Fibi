import { Component, OnInit } from '@angular/core';
import { NotesAttachmentsService } from './notes-attachments.service';
import { Subscription } from 'rxjs';
import { listAnimation } from '../../common/utilities/animations';
import { CommonService } from '../../common/services/common.service';
import { openSlider, closeSlider } from '../../common/utilities/custom-utilities';
import { EDITOR_CONFIGURATION, HTTP_ERROR_STATUS, HTTP_SUCCESS_STATUS } from '../../app-constants';
import * as DecoupledEditor from '@ckeditor/ckeditor5-build-decoupled-document';
import { subscriptionHandler } from '../../../../../fibi/src/app/common/utilities/subscription-handler';
import { deepCloneObject } from '../../../../../fibi/src/app/common/utilities/custom-utilities';

class Notes {
    content: string;
    noteId: number;
    personId: number;
    updateTimestamp: number;
    updateUser: string;
}
@Component({
    selector: 'app-notes-attachments',
    templateUrl: './notes-attachments.component.html',
    styleUrls: ['./notes-attachments.component.scss'],
    animations: [listAnimation]
})

export class NotesAttachmentsComponent implements OnInit {

    $subscriptions: Subscription[] = [];
    notesList: Array<Notes> = [];
    isFirstTimeLoad = false;
    showSlider = false;
    isEditMode = false;
    currentSelectedNote: Notes = new Notes();
    isOpenMoreMenu = [];
    public Editor = DecoupledEditor;
    editorConfig = EDITOR_CONFIGURATION;
    isSaving = false;

    constructor(private _noteAndAttachmentService: NotesAttachmentsService, private _commonService: CommonService) { }

    ngOnInit() {
        this.getNotesViaSubject();
        this.getNotesList();
    }

    private addGridBackground(isAdd): void {
        let body = document.getElementById('app-main-router');
        isAdd ? body.classList.add('grid-back') : body.classList.remove('grid-back');
    }

    private getNotesViaSubject(): void {
        this.$subscriptions.push(this._commonService.$updateLatestNote.subscribe((data: any) => {
            if (data) {
                this.notesList.unshift(data);
                if (this.notesList.length) {
                    this.addGridBackground(true);
                }
            }
        }));
    }

    private getNotesList(): void {
        this.isFirstTimeLoad = true;
        this.$subscriptions.push(this._noteAndAttachmentService.fetchAllNotesForPerson(this._commonService.getCurrentUserDetail('personId')).subscribe((data: any) => {
            this.notesList = data;
            if (this.notesList.length) {
                this.addGridBackground(true);
            }
            this.isFirstTimeLoad = false;
        }))
    }

    public onReady(editor) {
        editor.ui.getEditableElement().parentElement.insertBefore(
            editor.ui.view.toolbar.element,
            editor.ui.getEditableElement()
        );
    }

    openNote(note): void {
        this.$subscriptions.push(this._noteAndAttachmentService.getNoteBaseOnId(note.noteId).subscribe((data: any) => {
            if (data) {
                this.currentSelectedNote = data;
                this.showSlider = true;
                this.isEditMode = this.currentSelectedNote.personId == this._commonService.getCurrentUserDetail('personId');
                this.isOpenMoreMenu = [];
                setTimeout(() => {
                    openSlider('edit-note-slider');
                });
            }
        }, error => {
            this._commonService.showToast(HTTP_ERROR_STATUS, "Error in fetching note details, please try again.");
        }));
    }

    closeEditNoteSlider(): void {
        closeSlider('edit-note-slider');
        setTimeout(() => {
            this.showSlider = false;
            this.currentSelectedNote = new Notes();
        }, 500);
    }

    updateContent(): void {
        if (this.currentSelectedNote.content.trim() && !this.isSaving) {
            this.isSaving = true;
            this.$subscriptions.push(this._noteAndAttachmentService.saveOrUpdatePersonNote({
                personId: this._commonService.getCurrentUserDetail('personId'),
                content: this.currentSelectedNote.content.trim(),
                noteId: this.currentSelectedNote.noteId
            }).subscribe((data: any) => {
                this.isSaving = false;
                if (data) {
                    this.currentSelectedNote = data;
                    const UPDATE_iNDEX = this.notesList.findIndex(ele => ele.noteId === this.currentSelectedNote.noteId);
                    this.notesList.splice(UPDATE_iNDEX, 1);
                    this.notesList.unshift(deepCloneObject(this.currentSelectedNote));
                    this._commonService.showToast(HTTP_SUCCESS_STATUS, 'Note Updated Successfully.');
                }
            }, error => {
                this.isSaving = false;
                this._commonService.showToast(HTTP_ERROR_STATUS, 'Error in updating notes, please try again.');
            }));
        }
    }

    clearNote(): void {
        this.currentSelectedNote.content = '';
    }

    clearOtherIndex(index, flag): void {
        this.isOpenMoreMenu = [];
        if (flag) {
            this.isOpenMoreMenu[index] = flag
        }
    }

    ngOnDestroy(): void {
        this.addGridBackground(false);
        subscriptionHandler(this.$subscriptions);
    }

    getUnformattedContent(content): string {
        const SAMPLE_ELEMENT = document.createElement("div");
        SAMPLE_ELEMENT.innerHTML = content;
        const MIN_CONTENT = SAMPLE_ELEMENT.textContent || SAMPLE_ELEMENT.innerText;
        let FINAL_CONTENT = MIN_CONTENT.length > 300 ? MIN_CONTENT.slice(0, 300) + '...' : MIN_CONTENT;
        return FINAL_CONTENT.replace(/\s+/g, ' ').trim();
    }

}
