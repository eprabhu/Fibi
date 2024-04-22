export interface ValidationResponse {
    formBuilderId: number
    sectionId: number
    sectionName: string
    componentId: string
    navigationURL: string
    validationType: 'VE' | 'VW' | 'VM'
    validationMessage: string
}
