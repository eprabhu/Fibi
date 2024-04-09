import { NgModule } from '@angular/core';
import { CommonModule } from '@angular/common';
import { FormBuilderNavComponent } from '../form-editor/form-builder-nav/form-builder-nav.component';
import { FormIntegrationComponent } from '../form-editor/form-integration/form-integration.component';
import { FormEditorComponent } from '../form-editor/form-editor/form-editor.component';
import { FormAddtionalInformationComponent } from '../form-editor/form-addtional-information/form-addtional-information.component';
import { DragDropModule } from '@angular/cdk/drag-drop';
import { Routes, RouterModule } from '@angular/router';
import { SharedModule } from '../../../shared/shared.module';
import { FormsModule } from '@angular/forms';
import { FormPreviewComponent } from './form-preview/form-preview.component';

const routes: Routes = [
    {
        path: '', component: FormBuilderNavComponent,
        children: [
            { path: '', redirectTo: 'editor', pathMatch: 'full' },
            { path: 'editor', component: FormEditorComponent },
            { path: 'preview', component:FormPreviewComponent  },
            { path: 'integration', component: FormIntegrationComponent },
        ]
    }]

@NgModule({
    imports: [
        CommonModule,
        DragDropModule,
        RouterModule.forChild(routes),
        SharedModule,
        FormsModule

    ],
    declarations: [
        FormBuilderNavComponent,
        FormIntegrationComponent,
        FormEditorComponent,
        FormAddtionalInformationComponent,
        FormPreviewComponent
    ],

})
export class FormEditorModule { }
