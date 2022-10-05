function nestedness(M)

# this code computes the nestedness of a given incident matrix M 
# according to the definition given in 
# Fortuna, M.A., et al.: Coevolutionary dynamics shape the structure of bacteria‐phage infection networks. Evolution 1001-1011 (2019). 
# DOI 10.1111/evo.13731

    # Binarize the matrix
    B = M.>0.5
    B = convert(Matrix{UInt8}, B)

    # nestedness of rows
    nested_rows = 0
    for i=1:(size(B,1)-1) 
       j = i + 1
       while  j <= size(B,1)
       
                shared=sum(B[i,:].*B[j,:]) # sum of common interactions
                k_i = sum(B[i,:])
                k_j = sum(B[j,:])

                # Handle disconnected nodes 
                if !(k_i == 0 || k_j==0) 
                  min_shared = min(k_i,k_j) # min of the degrees
                  nested_rows = nested_rows + (shared/min_shared)
                end
                
            j = j + 1 # while iterator
        end
    end

    # nestedness of columns
    nested_columns = 0
    for i=1:(size(B,2) - 1) 
       j = i + 1 
       while  j <= size(B,2)
                shared=sum(B[:,i].*B[:,j]) # sum of common interactions
                k_i = sum(B[:,i])
                k_j = sum(B[:,j])

                # Handle disconnected nodes 
                if !(k_i == 0 || k_j==0) 
                  min_shared = min(k_i,k_j) # min of the degrees
                  nested_columns = nested_columns+(shared/min_shared)
                end   
                
            j = j + 1 # while iterator 
        end 
    end
    
    # nestedness of the network
    nestedness_network = (nested_rows+nested_columns)/((size(B,1)*(size(B,1)-1)/2)+(size(B,2)*(size(B,2)-1)/2))
    return nestedness_network
end