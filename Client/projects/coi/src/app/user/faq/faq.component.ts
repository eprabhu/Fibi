import { Component, OnInit } from '@angular/core';
import { Subscription } from 'rxjs';
import { faqService } from './faq.service';
import { deepCloneObject, fileDownloader } from '../../../../../fibi/src/app/common/utilities/custom-utilities';
import { subscriptionHandler } from '../../../../../fibi/src/app/common/utilities/subscription-handler';
import { fadeInOutHeight } from '../../common/utilities/animations';

declare const $: any;

@Component({
    selector: 'app-faq',
    templateUrl: './faq.component.html',
    styleUrls: ['./faq.component.scss'],
    animations: [fadeInOutHeight]
})

export class FaqComponent implements OnInit {
    buttonName: any;
    tempQuestionList: any = [];
    questionsList = [];
    triggerFaq = false;
    isCollapse: any = [];
    selectedQuestion = 0;
    searchText;
    faqDetails: any;
    categoryList: any;
    categoryName: any;
    selectedCategory = 0;
    faqRequestObject: any = {};
    $subscriptions: Subscription[] = [];
    isedit: boolean;
    question: any;
    answer: any;
    questionid: any;
    allQuestionData: any; // All Questions
    editIndex = -1;

    constructor(private _faqService: faqService) {
    }

    ngOnInit() {
        this.displayFAQ(1, '', true, 0);
    }

    ngOnDestroy() {
        subscriptionHandler(this.$subscriptions);
    }

    searchQuestions(searchText) {
        this.tempQuestionList = this.questionsList;
        if (searchText !== '') {
            this.tempQuestionList = this.tempQuestionList.filter(v => {
                return v.question.toLowerCase().includes(searchText.toLowerCase())
                    || v.answer.toLowerCase().includes(searchText.toLowerCase());
            });
        } else {
            this.tempQuestionList = this.questionsList;
        }
    }


    /**
       * @param  {} categoryName
       * returns the current category name
       */
    displayFAQ(categoryCode, subCategoryCode, isSelected = false, index,) {
        this.selectedCategory = index;
        this.faqRequestObject.categoryCode = categoryCode;
        this.faqRequestObject.subCategoryCode = subCategoryCode;
        this.$subscriptions.push(this._faqService.getFaq(this.faqRequestObject).subscribe((data: any) => {
            if (isSelected) this.categoryList = data.faqCategory;
            this.questionsList = data.faq;
            this.tempQuestionList = this.questionsList;
            this.categoryName = this.categoryList.find(type => type.categoryCode === categoryCode).description;
        }));
    }

    redirectUrl(url) {
        url.includes('http') ? window.open(url, '_blank') : window.open('//' + url, '_blank');
    }

    /**
     * @param  {} details
     * @param  {} attachmentId
     * to download attachments displayed with faq
     */
    saveAttachment(details, attachmentId) {
        this.$subscriptions.push(this._faqService.getAttachment(attachmentId)
            .subscribe((data: any) => {
                fileDownloader(data, details.fileName);
            },
                error => console.log('Error downloading the file.', error),
                () => { }));
    }

    /**
     * @param  {} elementId
     * stores index of question selected from  question list
     * specific faq content is displayed for each question selected
     */
    pageScroll(elementId) {
        const questionIndex = document.getElementById(elementId);
        if (questionIndex) {
            questionIndex.scrollIntoView({ behavior: 'smooth' });
        }
    }
    /**
     * to reset value of add faq trigger flag
     */
    resetFaqTrigger(event) {
        this.triggerFaq = event;
    }

    highlightQuestion(index) {
        this.selectedQuestion = index;
    }

    /**
     * @param  {} searchText
     * loads faq according to quesion or answer searched
     */
    loadQuestions(searchText) {
        this.$subscriptions.push(this._faqService.getFaq(searchText).subscribe((data: any) => {
            this.faqDetails = data;
        }));
    }
    // Used for editing
    editdata(data, index) {
        this.buttonName = 'Update'
        this.editIndex = index;
        this.allQuestionData = deepCloneObject(data);//question data
        $('#add-new-faq').modal('show');
    }
    // passing the data from the child(add-faq) to parent(faq)
    updatedata(event) {
        this.tempQuestionList.push(event);
    }

    addNewQuestion() {
        this.buttonName = 'Add Questions';
        this.allQuestionData = [];
    }
}
