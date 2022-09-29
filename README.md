# DICOM_TOOLS

MBA USP/ESALQ DATA SCIENCE AND ANALYTICS
DICOM TOOLKIT 

Este projeto foi desenvolvido como trabalho de conclusão de curso do aluno Luiz Castiglioni no curso do MBA - Data Science and ANalytics da USP / ESALQ
Trabalho desenvolvido em parceria com o departamento de AI da Escola Paulista de Medicina UNIFESP.

 ![usp](https://user-images.githubusercontent.com/87153755/192903945-c3f79221-d2be-41f2-98e7-b0161a85eb53.jpeg)    <------------------->   ![images](https://user-images.githubusercontent.com/87153755/192904060-adcf37ac-0d8f-4ec6-ab66-71482526845d.jpeg)


## Reconhecimento de objetos em imagens de Tomografia de Abdómen no formato DICOM. 

Foram selecionados 200 exames de tomografia de abdomen de 200 pacientes diferentes. 
Desses 200 exames foram geradas anotações pelo grupo de médicos e residentes da UNIFESP sobre a presença do órgão vesícula ou a presença do clipe cirúrgico em casos de pacientes que tiveram a vesícula retirada. Destas anotações surgiu a tabela annot_clean:


![annot_clean](https://user-images.githubusercontent.com/87153755/192905097-98bce90d-f05f-41a9-bc1a-665562182f4c.png)

## Tabela de Frequencia da Variável Categórica Nominal 'Hipodistendida/Vesícula normal/Clipe/sem clipe (HVSC)`

![tabela_frequencia](https://user-images.githubusercontent.com/87153755/192905780-bb9d48d3-5f41-4d0c-af1a-0d0375bfffaf.png)


Foram treinadas 2 labels para identificar a presença da vesícula, e a presença do clipes cirúrgicos colocado em pacientes que passam por cirurgia de retirada de órgãos.



## Resultados do Treinamento da Rede Convolucional baseada no algoritmo YOLO

![train_yolo_dicom](https://user-images.githubusercontent.com/87153755/192905949-452ef140-4991-4908-9ec4-d59095065f2b.png)

        


### CLIPE ###

 ![X0003886464_3_156](https://user-images.githubusercontent.com/87153755/192902529-de44497c-8cb0-4957-bece-b102fa433e67.png) 
 
### VESICULA ###
 
 ![X0000685389_4_149](https://user-images.githubusercontent.com/87153755/192903052-797254df-19a7-4ec4-86ae-8c59eb7469e2.png)



As imagens foram anonimizadas conforme protoco de pesquisa médica.

o peso gerado pela rede convolucional esta no branch master yolo_dicom_v1.pt

![results](https://user-images.githubusercontent.com/87153755/192903322-95b4a409-7217-453f-b94d-18a3191bc990.png)
